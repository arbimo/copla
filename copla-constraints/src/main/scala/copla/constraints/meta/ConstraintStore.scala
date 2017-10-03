package copla.constraints.meta

import copla.constraints.meta.constraints._
import copla.constraints.meta.events._
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.IVar

import scala.collection.mutable
import updates._
import cats.implicits._

class ConstraintStore(_csp: CSP, toClone: Option[ConstraintStore]) {
  implicit val csp = _csp

  def this(_csp: CSP) = this(_csp, None)

  val active: mutable.Set[Constraint] = toClone match {
    case None       => mutable.Set[Constraint]()
    case Some(base) => base.active.clone()
  }

  val satisfied: mutable.Set[Constraint] = toClone match {
    case None       => mutable.Set[Constraint]()
    case Some(base) => base.satisfied.clone()
  }

  private val activeConstraintsForVar: mutable.Map[IVar, mutable.Set[Constraint]] =
    toClone match {
      case None       => mutable.Map()
      case Some(base) => base.activeConstraintsForVar.map(kv => (kv._1, kv._2.clone()))
    }

  private val watchedConstraintsForVar: mutable.Map[IVar, mutable.Set[Constraint]] =
    toClone match {
      case None       => mutable.Map()
      case Some(base) => base.watchedConstraintsForVar.map(kv => (kv._1, kv._2.clone()))
    }

  private val watchers: mutable.Map[Constraint, mutable.Set[Constraint]] = toClone match {
    case None       => mutable.Map()
    case Some(base) => base.watchers.map(kv => (kv._1, kv._2.clone()))
  }

  private val watches: mutable.Map[Constraint, mutable.Set[Constraint]] = toClone match {
    case None       => mutable.Map()
    case Some(base) => base.watches.map(kv => (kv._1, kv._2.clone()))
  }

  /** Storage to keep mutable data structures of constraints */
  private val datas: mutable.Map[Constraint, ConstraintData] = toClone match {
    case Some(base) => base.datas.map(p => (p._1, p._2.clone))
    case None       => mutable.Map()
  }

  /** Records a new active constraint and adds its variables to the index */
  private def record(constraint: Constraint) {
    if(!constraint.active) {
      active += constraint
      for (v <- constraint.variables) {
        activeConstraintsForVar.getOrElseUpdate(v, mutable.Set()) += constraint
      }
    }
  }

  /** Removes a constraint from the active list and removes it from the variable index */
  private def onSatisfaction(constraint: Constraint) {
    if (active.contains(constraint)) {
      active -= constraint
      satisfied += constraint
      for (v <- constraint.variables) {
        assert2(activeConstraintsForVar.contains(v) && activeConstraintsForVar(v).contains(constraint))
        activeConstraintsForVar(v) -= constraint
        if (activeConstraintsForVar(v).isEmpty)
          activeConstraintsForVar -= v
      }
    }
    for (watched <- monitoredBy(constraint).toList)
      removeWatcher(watched, constraint)
  }

  def addWatcher(constraint: Constraint, watcher: Constraint): Update = {
    val introductionResult =
      if (!watchers.contains(constraint)) {
        // constraint is not watched yet, record its variable and notify other components
        watchers.put(constraint, mutable.Set())
        for (v <- constraint.variables)
          watchedConstraintsForVar.getOrElseUpdate(v, mutable.Set()) += constraint

        csp.addEvent(WatchConstraint(constraint)) >>
          foreach(constraint.onWatch) {
            case Watch(subConstraint) =>
              addWatcher(subConstraint, constraint)
          }
      } else {
        consistent
      }
    introductionResult >> check {
      watchers(constraint) += watcher
      watches.getOrElseUpdate(watcher, mutable.Set()) += constraint
      assert2(watchers(constraint).contains(watcher))
      assert2(watches(watcher).contains(constraint))
    }
  }

  private def removeWatcher(constraint: Constraint, watcher: Constraint) {
    assert1(watchers.contains(constraint))
    assert1(watches.contains(watcher))
    assert1(watches(watcher).contains(constraint))
    watches(watcher) -= constraint
    watchers(constraint) -= watcher
    if (watchers(constraint).isEmpty) {
      // nobody is watching it anymore, remove variable watches and notify other components
      for (v <- constraint.variables) {
        watchedConstraintsForVar(v) -= constraint
        if (watchedConstraintsForVar(v).isEmpty)
          watchedConstraintsForVar -= v
      }
      watchers -= constraint
      csp.addEvent(UnwatchConstraint(constraint))
    }
    if (watches(watcher).isEmpty)
      watches -= watcher
  }

  def activeWatching(variable: IVar): Iterable[Constraint] =
    activeConstraintsForVar.getOrElse(variable, Nil)

  def monitoredWatching(variable: IVar): Iterable[Constraint] =
    watchedConstraintsForVar.getOrElse(variable, Nil)

  /** All constraints monitoring "constraint" */
  def monitoring(constraint: Constraint): Iterable[Constraint] =
    watchers.getOrElse(constraint, Nil)

  /** All constraints monitored by "constraint" */
  def monitoredBy(constraint: Constraint): Iterable[Constraint] =
    watches.getOrElse(constraint, Nil)

  def isActive(constraint: Constraint) = active.contains(constraint)

  def isWatched(constraint: Constraint) = watchers.contains(constraint)

  /** All constraints that have been posted (not including the ones that are watched) */
  def all = active ++ satisfied

  /** All constraints that are currently being monitored */
  def watched = watchers.keys

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
