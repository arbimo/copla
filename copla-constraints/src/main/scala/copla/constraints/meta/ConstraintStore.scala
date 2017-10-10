package copla.constraints.meta

import copla.constraints.meta.constraints._
import copla.constraints.meta.events._
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.IVar

import scala.collection.mutable
import updates._
import copla.constraints.meta.dependencies._

import scala.collection.immutable.Set

class ConstraintStore(_csp: CSP, toClone: Option[ConstraintStore]) {
  implicit val csp = _csp

  def this(_csp: CSP) = this(_csp, None)

  val satisfied: mutable.Set[Constraint] = toClone match {
    case None       => mutable.Set[Constraint]()
    case Some(base) => base.satisfied.clone()
  }

  val depTree: DependencyTree = toClone match {
    case Some(base) => base.depTree.clone()
    case None       => new DependencyTree()
  }

  val childrenOf: Node => Set[Node] = {
    case Var(v) => Set()
    case Cst(c) => c.variables.map(Var)
  }

  /** Storage to keep mutable data structures of constraints */
  private val datas: mutable.Map[Constraint, ConstraintData] = toClone match {
    case Some(base) => base.datas.map(p => (p._1, p._2.clone))
    case None       => mutable.Map()
  }

  def treatAdditionalNodes(nodes: Iterable[Node]): Update =
    foreach(nodes) {
      case Var(_) => consistent
      case Cst(c) => csp.addEvent(WatchConstraint(c))
    }

  def treatRemovedNodes(nodes: Iterable[Node]): Update =
    foreach(nodes) {
      case Var(_) => consistent
      case Cst(c) => csp.addEvent(UnwatchConstraint(c))
    }

  /** Records a new active constraint and adds its variables to the index */
  private def record(constraint: Constraint) {
    val additions = depTree.addEdge(Root, Cst(constraint), childrenOf)
    treatAdditionalNodes(additions)
  }

  /** Removes a constraint from the active list and removes it from the variable index */
  private def onSatisfaction(constraint: Constraint) {
    val removals = depTree.removeEdge(Root, Cst(constraint))
    satisfied += constraint
    treatRemovedNodes(removals)
  }

  def addWatcher(constraint: Constraint, watcher: Constraint): Update = {
    val additions = depTree.addEdge(Cst(watcher), Cst(constraint), childrenOf)
    treatAdditionalNodes(additions)
  }

  private def removeWatcher(constraint: Constraint, watcher: Constraint) {
    val removals = depTree.removeEdge(Cst(watcher), Cst(constraint))
    treatRemovedNodes(removals)
  }

  def watching(variable: IVar): Iterable[Constraint] =
    depTree.parents(Var(variable)).collect { case Cst(c) => c }

  def active: Iterable[Constraint] =
    depTree.children(Root).collect { case Cst(c) => c }

  def watched: Iterable[Constraint] =
    depTree.nodes.collect { case Cst(c) if isWatched(c) => c }

  /** All constraints monitoring "constraint" */
  def monitoring(constraint: Constraint): Iterable[Constraint] =
    depTree.parents(Cst(constraint)).collect { case Cst(c) => c }

  /** All constraints monitored by "constraint" */
  def monitoredBy(constraint: Constraint): Iterable[Constraint] =
    depTree.children(Cst(constraint)).collect { case Cst(c) => c }

  def isActive(constraint: Constraint) = depTree.children(Root).contains(Cst(constraint))

  def isWatched(constraint: Constraint) = depTree.parents(Cst(constraint)).toSeq match {
    case Seq()     => false
    case Seq(Root) => false
    case _         => true
  }

  /** All constraints that have been posted (not including the ones that are watched) */
  def all = active ++ satisfied

  /** Handle events that should be handled before all other components,
    * mainly to record new constraints. */
  def handleEventFirst(event: Event) = {
    event match {
      case Satisfaction(constraint) =>
        onSatisfaction(constraint)
      case NewConstraint(constraint) =>
        record(constraint)
      case DomainExtended(v) =>
        // TODO: should index satisfied constraints by variable
        for (c <- satisfied.clone()) {
          if (c.variables.contains(v)) {
            // repost constraint
            satisfied -= c
            csp.post(c)
          }
        }
      case _ =>
    }
  }

  /** Handle events that should be handled after all other components,
    * mainly to clean up on satisfaction of watch constraints. */
  def handleEventLast(event: Event) {
    event match {
      case e: WatchedSatisfactionUpdate =>
        if (e.constraint.watched)
          while (monitoring(e.constraint).nonEmpty) // defensive copy as the list will be modified
          removeWatcher(e.constraint, monitoring(e.constraint).head)
        assert1(!e.constraint.watched)
      case UnwatchConstraint(c) =>
        // constraint is not watched anymore, remove all remove all subwatches of this constraint
        while (monitoredBy(c).nonEmpty) removeWatcher(monitoredBy(c).head, c)
      case _ =>
    }
  }

  /** Records the data field associated to this constraint */
  def setDataOf[T <: ConstraintData](constraint: Constraint with WithData[T], value: T): Update = check {
    datas.put(constraint, value)
  }

  /** Returns true iff a data field was previously recorded for this constraint */
  def hasDataOf(constraint: Constraint with WithData[_]): Boolean = datas.contains(constraint)

  /** Returns the data previously recorded for this constraint. */
  def dataOf[T <: ConstraintData](constraint: Constraint with WithData[T]): T =
    datas(constraint).asInstanceOf[T]

  def clone(newCSP: CSP): ConstraintStore = new ConstraintStore(newCSP, Some(this))
}
