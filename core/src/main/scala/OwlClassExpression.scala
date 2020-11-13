import org.semanticweb.owlapi.model.{IRI, OWLClass, OWLClassExpression}

import scala.collection.JavaConverters._
import java.util.stream.StreamSupport
import java.util.stream.{Stream => jStream}

object OwlClassExpression {

  def toOwlClassExpression(c: ClassExpression,
                           api: OwlApi): OWLClassExpression =
    c match {
      case ClassExpression.Universal =>
        api.getOWLThing

      case ClassExpression.Empty =>
        api.getOWLNothing

      case ClassExpression.Singleton(encapsulated) =>
        api.getOWLClass(IRI.create(encapsulated))

      case ClassExpression.Complement(e) =>
        api.getOWLObjectComplementOf(toOwlClassExpression(e, api))

      case ClassExpression.Difference(a, b) =>
        toOwlClassExpression(a.intersection(b.complement()), api)

      case ClassExpression.Intersection(s) =>
        val ce: jStream[OWLClassExpression] = StreamSupport.stream(
          s.map(toOwlClassExpression(_, api))
            .toSet  // stainless
            .theSet // stainless
            .asJava
            .spliterator(),
          false)
        api.getOWLObjectIntersectionOf(ce)

      case ClassExpression.Union(s) =>
        val ce: jStream[OWLClassExpression] = StreamSupport.stream(
          s.map(toOwlClassExpression(_, api))
            .toSet  // stainless
            .theSet // stainless
            .asJava
            .spliterator(),
          false)
        api.getOWLObjectUnionOf(ce)
    }
}
