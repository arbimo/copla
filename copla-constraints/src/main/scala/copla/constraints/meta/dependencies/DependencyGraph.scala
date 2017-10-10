package copla.constraints.meta.dependencies

import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.stn.variables.Timepoint
import copla.constraints.meta.variables.IVar

import scala.collection.mutable

sealed trait RootOrNode
/** Root of the DAG */
case object Root extends RootOrNode
sealed trait Node extends RootOrNode

/** Constraint node, outgoing edges might target:
  * (i) other constraints for which the constraint requires notification of changes.
  * (ii) Variables on which it depends.
  */
case class Cst(c: Constraint) extends Node

/** Variable node.
  * A variable node representing temporal information might have dependencies to a Time node.
  */
case class Var(v: IVar) extends Node

/** Represents a temporal delay between two timepoints. */
case class Time(from: Timepoint, to: Timepoint) extends Node


/**
  * A Directed Acyclic graph keep track of dependencies between constraints.
  *
  * The graph is incrementally maintained such that all nodes are reachable from the Root
  */
class DependencyGraph(optBase: Option[DependencyGraph] = None) {

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
    if(from.isInstanceOf[Node] && numChildren(from) == 0)
      tree -= from
  }

  def numChildren(node: RootOrNode): Int = tree.get(node).map(_.size).getOrElse(0)
  def numParents(node: Node): Int = inverseTree.get(node).map(_.size).getOrElse(0)

  def nodes: Iterable[Node] = inverseTree.keySet

  def children(node: RootOrNode): collection.Set[Node] = tree.getOrElse(node, Set())
  def parents(node: Node): collection.Set[RootOrNode] = inverseTree.getOrElse(node, Set())

  def contains(node: RootOrNode): Boolean = node match {
    case Root => assert(tree.contains(Root)); true
    case x: Node => inverseTree.contains(x)
  }

  /** Add an edge to the tree. If the target node is not present, then its children are
    * recursively generated and added to the graph by quering the given function.
    *
    * @return All nodes that were added to the graph. */
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

  /**
    * Removes an edge from the graph. If the target of the edge is no longer attached to the root,
    * then it is remove from the graph together with all its children that are no longer reachable from the root.
    * @return A list of all nodes that were removed from the graph.
    */
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

  override def clone(): DependencyGraph = new DependencyGraph(Some(this))

}


object DependencyGraph {

}
