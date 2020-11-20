import stainless.annotation._
import stainless.lang._
import stainless.collection._

case class Adjascent[V](v: V, adj: List[V]) {
  require(!adj.contains(v) && ListUtils.noDuplicate(adj))
}

case class DirectedGraph[V](
    adjascent: List[Adjascent[V]] = List[Adjascent[V]]()) {
  require(DirectedGraph.wff(adjascent))

  def vertices: List[V] = {
    adjascent.map(_.v)
  } ensuring (r =>
    ListUtils.noDuplicate(r) &&
      r.size == adjascent.size)

  def addVertex(v: V): DirectedGraph[V] = {
    val adj = DirectedGraph.addVertex(v, adjascent, List[Adjascent[V]]())
    DirectedGraph(adj)
  } ensuring (r =>
    r.vertices.contains(v) &&
      ListUtils.noDuplicate(r.adjascent.map(_.v)))

  def addEdge(from: V, to: V): DirectedGraph[V] = {
    require(from != to)
    val g = this.addVertex(from).addVertex(to)
    val adj = DirectedGraph.addEdge(from, to, g.adjascent, List[Adjascent[V]]())
    DirectedGraph(adj)
  }
//  ensuring (r =>
//    r.vertices.contains(from) &&
//      r.vertices.contains(to) &&
//      ListUtils.noDuplicate(r.adjascent.map(_.v)) &&
//      r.adjascent.exists(a => a.v == from && a.adj.contains(to)))

  def childrenOf(v: V): List[V] = {
    adjascent.find(_.v == v).getOrElse(Adjascent[V](v, List[V]())).adj
  } ensuring (r => r.forall(vertices.contains))

  def descendantsOf(v: V): List[V] = {
    require(vertices.contains(v))
    var result = Set[V]()
    var dfs = DFSIterator.make(this, v)
    while (dfs.hasNext()) {
      decreases(dfs.size)
      val (v, next) = dfs.next()
      result = result + v
      dfs = next
    }
    (result - v).toList
  } //ensuring (r => DirectedGraph.includesAll(adjascent, r))

}

object DirectedGraph {

  def wff[V](adjascent: List[Adjascent[V]]): Boolean =
    unique(adjascent) && adjascent.forall(a => a.adj.forall(to => adjascent.exists(_.v == to)))

  def unique[V](adjascent: List[Adjascent[V]]): Boolean =
    ListUtils.noDuplicate(adjascent.map(_.v))

  def includesAll[V](adjascent: List[Adjascent[V]], vs: List[V]): Boolean =
    vs.forall(v => adjascent.exists(_.v == v))

  def excludes[V](adjascent: List[Adjascent[V]], v: V): Boolean =
    adjascent.forall(_.v != v)

  def addVertex[V](v: V,
                   adj: List[Adjascent[V]],
                   acc: List[Adjascent[V]]): List[Adjascent[V]] = {
    require(unique(adj) && unique(acc) && wff(adj ++ acc) && excludes(acc, v))
    decreases(adj)
    adj match {
      case Nil() =>
        Adjascent[V](v, List[V]()) :: acc
      case ::(hd, tl) =>
        if (hd.v == v)
          adj ++ acc
        else
          addVertex(v, tl, hd :: acc)
    }
  } ensuring (r =>
    wff(r) &&
    r.exists(_.v == v))

  def addEdge[V](from: V,
                 to: V,
                 adj: List[Adjascent[V]],
                 acc: List[Adjascent[V]]): List[Adjascent[V]] = {
    require(from != to && unique(adj) && unique(acc) && wff(adj ++ acc) && includesAll(adj, List[V](from, to)) && excludes(acc, from))
    decreases(adj)
    adj match {
      case ::(hd, tl) =>
        if (hd.v == from) {
          if (hd.adj.contains(to))
            adj ++ acc
          else
            Adjascent[V](from, to :: hd.adj) :: tl ++ acc
        } else
          addEdge(from, to, tl, hd :: acc)
    }
  } ensuring (r => wff(r) && r.exists(a => a.v == from && a.adj.contains(to)))
}
