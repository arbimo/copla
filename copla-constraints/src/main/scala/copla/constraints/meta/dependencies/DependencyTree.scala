package copla.constraints.meta.dependencies

import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.variables.IVar

import scala.collection.mutable

sealed trait RootOrNode
case object Root extends RootOrNode
sealed trait Node extends RootOrNode
case class Cst(c: Constraint) extends Node
case class Var(v: IVar) extends Node




class DependencyTree(optBase: Option[DependencyTree] = None) {

  val tree: mutable.Map[RootOrNode, mutable.Set[Node]] = optBase match {
    case Some(base) => mutable.Map(base.tree.mapValues(_.clone).toSeq: _*)
    case None => mutable.Map(Root -> mutable.Set())
  }

  val inverseTree: mutable.Map[Node, mutable.Set[RootOrNode]] = optBase match {
    case Some(base) => mutable.Map(base.inverseTree.mapValues(_.clone).toSeq: _*)
    case None => mutable.Map()
  }

  private def addEdgeUnsafe(from: RootOrNode, to: Node): Unit = {
    tree.getOrElseUpdate(from, mutable.Set()) += to
    inverseTree.getOrElseUpdate(to, mutable.Set()) += from
  }
  private def deleteEdgeUnsafe(from: RootOrNode, to: Node): Unit = {
    tree.getOrElse(from, mutable.Set()) -= to
    inverseTree.getOrElse(to, mutable.Set()) -= from
  }

  def numChildren(node: Node): Int = tree.get(node).map(_.size).getOrElse(0)
  def numParents(node: Node): Int = inverseTree.get(node).map(_.size).getOrElse(0)

  def nodes: Iterable[Node] = inverseTree.keySet

  def children(node: RootOrNode): Set[Node] = tree.get(node).map(_.toSet).getOrElse(Set())
  def parents(node: Node): Set[RootOrNode] = inverseTree.get(node).map(_.toSet).getOrElse(Set())

  def contains(node: RootOrNode): Boolean = node match {
    case Root => assert(tree.contains(Root)); true
    case x: Node => inverseTree.contains(x)
  }

  def addEdge(from: RootOrNode, to: Node, children: Node => Set[Node]): Seq[Node] = {
    require(contains(from))
    if(contains(to)) {
      addEdgeUnsafe(from, to)
      Nil
    } else {
      addEdgeUnsafe(from, to)
      val recursivelyAdded =
        for(child <- children(to).toSeq ; n <- addEdge(to, child, children)) yield n
      recursivelyAdded :+ to
    }
  }

  def removeEdge(from: RootOrNode, to: Node): Seq[Node] = {
    deleteEdgeUnsafe(from, to)
    numParents(to) match {
      case 0 =>
        val subnodes = children(to).toSeq
        val recursivelyRemoved =
          for (sub <- subnodes) yield removeEdge(to, sub)
        assert(numParents(to) == 0)
        assert(numChildren(to) == 0)
        inverseTree -= to
        recursivelyRemoved.flatten :+ to
      case _ =>
        // node still has parents, don't touch anything.
        Nil
    }
  }

  override def clone(): DependencyTree = new DependencyTree(Some(this))

}


object DependencyTree {

}
