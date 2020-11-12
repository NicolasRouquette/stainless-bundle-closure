import stainless.lang._
import stainless.collection._

/**
 * ClassExpression defines methods for constructing class expressions,
 * including singleton expressions encapsulating a single class, complements, intersections,
 * and unions. While the library does not perform any mathematical reasoning, it employs
 * these theorems to simplify expressions:
 * <ul>
 * <li>Theorem 1: For any class A, (A&prime;)&prime; = A.</li>
 *
 * <li>Theorem 2: For any class A, A &cap; A = A.</li>
 *
 * <li>Theorem 3: For any classes A and B, A &cap; B = B &cap; A.</li>
 *
 * <li>Theorem 4: For any classes A, B, and C,
 * (A &cap; B) &cap; C = A &cap; (B &cap; C).</li>
 *
 * <li>Theorem 5: For any class A, A &cup; A = A.</li>
 *
 * <li>Theorem 6: For any classes A and B, A &cup; B = B &cup; A.</li>
 *
 * <li>Theorem 7: For any classes A, B, and C,
 * (A &cup; B) &cup; C = A &cup; (B &cup; C).</li>
 *
 * <li>Theorem 8: For any classes A, B, and C, (A\B)\C = A\(B &cup; C).</li>
 *
 * <li>Theorem 9: For any class A and empty set &empty;, &empty; &cap; A = &empty;.</li>
 *
 * <li>Theorem 10: For any class A, &empty; &cup; A = A.</li>
 *
 * <li>Theorem 11: For any class A, A\&empty; = A.</li>
 *
 * <li>Theorem 12: For any class A, &empty;\A = &empty;.</li>
 *
 * <li>Theorem 13: For any class A, A\A = &empty;.</li>
 *
 * <li>Theorem 14: For any class A and universal set U, U &cap; A = A.</li>
 *
 * <li>Theorem 15: For any class A, U &cup; A = U.</li>
 *
 * <li>Theorem 16: For any class A, A\U = &empty;.</li>
 *
 * <li>Theorem 17: &empty;&prime; = U.</li>
 *
 * <li>Theorem 18: U&prime; = &empty;.</li>
 *
 * <li>Theorem 19: For any class A, A&prime;\A = A&prime;.</li>
 *
 * <li>Theorem 20: For any class A, A&prime; &cap; A = &empty;.</li>
 *
 * <li>Theorem 21: For any class A, A&prime; &cup; A = U.</li>
 * </ul>
 *
 * @author Steven Jenkins j.s.jenkins@jpl.nasa.gov
 * @author Nicolas Rouquette nicolas.f.rouquette@jpl.nasa.gov
 * @version 0.0.1
 * @since 0.0.1
 */
sealed trait ClassExpression {
  def complement(): ClassExpression
  def difference(e: ClassExpression): ClassExpression
  def intersection(e: ClassExpression): ClassExpression
  def union(e: ClassExpression): ClassExpression
}

case object Universal extends ClassExpression {
  override def complement(): ClassExpression = {
    // Theorem 18
    Empty
  }

  override def difference(e: ClassExpression): ClassExpression =
    Difference(this, e)

  override def intersection(e: ClassExpression): ClassExpression = {
    // Theorem 14
    e
  }

  override def union(e: ClassExpression): ClassExpression = {
    // Theorem 15
    Universal
  }
}

case object Empty extends ClassExpression {
  override def complement(): ClassExpression = {
    // Theorem 17
    Universal
  }

  override def difference(e: ClassExpression): ClassExpression = {
    // Theorem 12
    Empty
  }

  override def intersection(e: ClassExpression): ClassExpression = {
    // Theorem 9
    Empty
  }

  override def union(e: ClassExpression): ClassExpression = {
    // Theorem 10
    e
  }
}

case class Singleton(encapsulated: Int) extends ClassExpression {
  override def complement(): ClassExpression = Complement(this)

  override def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 11
        this
      case Universal =>
        // Theorem 16
        Empty
      case _ =>
        if (this == e) {
          // Theorem 13
          Empty
        } else
          Difference(this, e)
    }

  override def intersection(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 2
      this
    } else e match {
      case Universal =>
        // Theorem 14
        this
      case Empty =>
        // Theorem 9
        Empty
      case _ =>
        Intersection(Cons(this, Cons(e, Nil())))
    }

  override def union(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 5
      this
    } else e match {
      case Universal =>
        // Theorem 15
        Universal
      case Empty =>
        // Theorem 10
        this
      case _ =>
        Union(Cons(this, Cons(e, Nil())))
    }
}

sealed trait Unary extends ClassExpression

case class Complement(c: ClassExpression) extends Unary {
  override def complement(): ClassExpression = c

  override def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 11
        this
      case Universal =>
        // Theorem 16
        Empty
      case _ =>
        if (this == e) {
          // Theorem 13
          Empty
        } else if (c == e) {
          // Theorem 19
          this
        } else
          Difference(this, e)
    }

  override def intersection(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 9
        Empty
      case Universal =>
        // Theorem 14
        this
      case _ =>
        if (this == e) {
          // Theorem 2
          this
        } else if (c == e) {
          // Theorem 20
          Empty
        } else
          Intersection(Cons(this, Cons(e, Nil())))
    }

  override def union(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 10
        this
      case Universal =>
        // Theorem 15
        Universal
      case _ =>
        if (this == e) {
          // Theorem 5
          this
        } else if (c == e) {
          // Theorem 21
          Universal
        } else
          Union(Cons(this, Cons(e, Nil())))
    }
}

sealed trait Binary extends ClassExpression {
  val a, b: ClassExpression
}

case class Difference(override val a: ClassExpression,
                      override val b: ClassExpression) extends Binary {
  override def complement(): ClassExpression = Complement(this)

  override def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 11
        this
      case Universal =>
        // Theorem 16
        Empty
      case _ =>
        if (e == this) {
          // Theorem 13
          Empty
        } else
          Difference(this, e)
    }

  override def intersection(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 2
      this
    } else e match {
      case Universal =>
        // Theorem 14
        this
      case Empty =>
        // Theorem 9
        Empty
      case _ =>
        Intersection(Cons(this, Cons(e, Nil())))
    }

  override def union(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 5
      this
    } else e match {
      case Universal =>
        // Theorem 15
        Universal
      case Empty =>
        // Theorem 10
        this
      case _ =>
        Union(Cons(this, Cons(e, Nil())))
    }
}

sealed trait Nary extends ClassExpression {
  val s : List[ClassExpression]

  override def complement(): ClassExpression = Complement(this)

  override def difference(e: ClassExpression): ClassExpression =
    e match {
      case Empty =>
        // Theorem 11
        this
      case Universal =>
        // Theorem 16
        Empty
      case _ =>
        if (e == this) {
          // Theorem 13
          Empty
        } else
          Difference(this, e)
    }
}

case class Intersection(override val s: List[ClassExpression]) extends Nary {

  override def intersection(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 2
      this
    } else e match {
      case Universal =>
        // Theorem 14
        this
      case Empty =>
        // Theorem 9
        Empty
      case Intersection(t) =>
        Intersection((s ++ t).unique)
      case _ =>
        Intersection(e :: s)
    }

  override def union(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 5
      this
    } else e match {
      case Universal =>
        // Theorem 15
        Universal
      case Empty =>
        // Theorem 10
        this
      case _ =>
        Union(Cons(this, Cons(e, Nil())))
    }
}

case class Union(override val s: List[ClassExpression]) extends Nary {

  override def intersection(e: ClassExpression): ClassExpression =
    if (this == e) {
      // Theorem 2
      this
    } else e match {
      case Universal =>
        // Theorem 14
        this
      case Empty =>
        // Theorem 9
        Empty
      case _ =>
        Intersection(Cons(this, Cons(e, Nil())))
    }

  override def union(e: ClassExpression): ClassExpression =
    e match {
      case Union(u) =>
        Union((u ++ s).unique)
      case _ =>
        Union(e :: s)
    }
}
