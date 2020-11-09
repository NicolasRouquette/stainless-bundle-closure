import stainless.lang._
import stainless.collection._

sealed trait ClassExpression {
  def complement(): ClassExpression = Complement(this)
  def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        this
      case Universal =>
        Empty
      case _ =>
        if (this == e)
          Empty
        else
          Difference(this, e)
    }
  def intersection(e: ClassExpression): ClassExpression =
    if (this == e)
      this
    else e match {
      case Universal =>
        e.intersection(this)
      case _: Intersection =>
        e.intersection(this)
      case Empty =>
        e.intersection(this)
      case _ =>
        Intersection(Cons(this, Cons(e, Nil())))
    }
  def union(e: ClassExpression): ClassExpression =
    if (this == e)
      this
    else e match {
      case Universal =>
        e.union(this)
      case _: Union =>
        e.union(this)
      case Empty =>
        e.union(this)
      case _ =>
        Union(Cons(this, Cons(e, Nil())))
    }
}

case object Universal extends ClassExpression {
  override def complement(): ClassExpression =
    Empty
  override def intersection(e: ClassExpression): ClassExpression =
    e
  override def union(e: ClassExpression): ClassExpression =
    e
}

case object Empty extends ClassExpression {
  override def complement(): ClassExpression =
    Universal
  override def intersection(e: ClassExpression): ClassExpression =
    Empty
  override def union(e: ClassExpression): ClassExpression =
    e
}

case class Singleton(encapsulated: Int) extends ClassExpression

sealed trait Unary extends ClassExpression

case class Complement(e: ClassExpression) extends Unary {
  override def complement(): ClassExpression = e
}

sealed trait Binary extends ClassExpression {
  val a, b: ClassExpression
}

case class Difference(override val a: ClassExpression,
                      override val b: ClassExpression) extends Binary {
  override def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        this
      case Universal =>
        Empty
      case _ =>
        if (e == this)
          Empty
        else
          Difference(a, b.union(e))
    }
}

sealed trait Nary extends ClassExpression {
  val s : List[ClassExpression]
}

case class Intersection(override val s: List[ClassExpression]) extends Nary {
  override def intersection(e: ClassExpression): ClassExpression =
    e match {
      case Intersection(t) =>
        Intersection((s ++ t).unique)
      case _ =>
        Intersection(e :: s)
    }
}

case class Union(override val s: List[ClassExpression]) extends Nary {

  override def union(e: ClassExpression): ClassExpression =
    e match {
      case Union(u) =>
        Union((u ++ s).unique)
      case _ =>
        Union(e :: s)
    }
}
