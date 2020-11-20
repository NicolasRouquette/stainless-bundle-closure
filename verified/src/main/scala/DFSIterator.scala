import stainless.annotation._
import stainless.lang._
import stainless.collection._

@inlineInvariant
case class DFSIterator[V](g: DirectedGraph[V],
                          stack: List[V],
                          toVisit: List[V]) {
  require(
    DirectedGraph.wff(g.adjascent) &&
      DirectedGraph.includesAll(g.adjascent, stack) &&
      DirectedGraph.includesAll(g.adjascent, toVisit) &&
      ListUtils.noDuplicate(toVisit) &&
      stack.forall(v => toVisit.contains(v)))

  def size: BigInt = toVisit.length

  def hasNext(): Boolean = stack.nonEmpty

  def pop(): Tuple2[V, DFSIterator[V]] = {
    require(hasNext() && DirectedGraph.wff(g.adjascent))
    stack match {
      case ::(v, ss) =>
        assert(g.vertices.contains(v))
        assert(ss.forall(v => g.vertices.contains(v)))
        assert(toVisit.contains(v))
        val rest: List[V] = toVisit - v
        assert(rest.size <= toVisit.size)
        (v, DFSIterator(g, ss, rest))
    }
  }
//  ensuring (r =>
//    g == r._2.g &&
//      r._2.size <= this.size)

  def next(): Tuple2[V, DFSIterator[V]] = {
    require(hasNext() && DirectedGraph.wff(g.adjascent))

    val (v, n) = pop()
    val vs = n.g.childrenOf(v)
    val vss = n.stack ++ vs
    (v, DFSIterator(n.g, vss, n.toVisit))
  }
//  ensuring (r =>
//    g == r._2.g &&
//      r._2.g.vertices.contains(r._1) &&
//      r._2.size < this.size)
}

object DFSIterator {
  def make[V](g: DirectedGraph[V], v: V): DFSIterator[V] = {
    require(g.adjascent.exists(_.v == v))
    DFSIterator[V](g, List[V](v), g.vertices)
  } ensuring (r =>
    r.stack.forall(v => g.adjascent.exists(_.v == v)) &&
      r.toVisit.forall(v => g.adjascent.exists(_.v == v)))
}
