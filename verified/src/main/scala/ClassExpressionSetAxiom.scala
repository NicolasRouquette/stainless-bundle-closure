import stainless.lang._
import stainless.collection._

sealed trait ClassExpressionSetAxiom {
  val set: List[ClassExpression]
}

object ClassExpressionSetAxiom {

  case class DisjointClassesAxiom(override val set: List[ClassExpression])
      extends ClassExpressionSetAxiom

  case class EquivalentClassesAxiom(override val set: List[ClassExpression])
      extends ClassExpressionSetAxiom

  case class DisjointUnionAxiom(c: ClassExpression.Singleton,
                                override val set: List[ClassExpression])
      extends ClassExpressionSetAxiom

}
