import org.semanticweb.owlapi.model.{OWLAxiom, OWLClass, OWLClassExpression}

import scala.collection.JavaConverters._
import java.util.stream.StreamSupport
import java.util.stream.{Stream => jStream}

object OwlAxiom {
  def toOwlAxiom(axiom: ClassExpressionSetAxiom, api: OwlApi): OWLAxiom = {

    val oces: jStream[OWLClassExpression] = StreamSupport.stream(
      axiom.set
        .map((ce: ClassExpression) =>
          OwlClassExpression.toOwlClassExpression(ce, api))
        .toSet  // stainless
        .theSet // stainless
        .asJava
        .spliterator(),
      false)

    axiom match {
      case _: ClassExpressionSetAxiom.DisjointClassesAxiom =>
        api.getOWLDisjointClassesAxiom(oces)

      case _: ClassExpressionSetAxiom.EquivalentClassesAxiom =>
        api.getOWLEquivalentClassesAxiom(oces)

      case ClassExpressionSetAxiom.DisjointUnionAxiom(c, _) =>
        OwlClassExpression.toOwlClassExpression(c, api) match {
          case cl: OWLClass =>
            api.getOWLDisjointUnionAxiom(cl, oces)
          case other =>
            throw new IllegalArgumentException(
              s"DisjointUnionAxiom(c=$c): c converts to a ${other.getClassExpressionType.getName} instead of a Class!")
        }

    }
  }
}
