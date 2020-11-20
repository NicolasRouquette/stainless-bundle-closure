
import stainless.collection._
import stainless.annotation._
import stainless.lang._
import stainless.proof._

// @see https://github.com/epfl-lara/verifythis2020/blob/master/src/main/scala/pgp/ListUtils.scala
object ListUtils {

  def noDuplicate[T](l: List[T]): Boolean = l match {
    case Nil() => true
    case Cons(x, xs) => !xs.contains(x) && noDuplicate(xs)
  }

}
