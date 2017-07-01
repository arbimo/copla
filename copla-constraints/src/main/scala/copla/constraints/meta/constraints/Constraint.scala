package copla.constraints.meta.constraints

import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

trait Constraint {

  type Satisfaction = ConstraintSatisfaction

  def variables(implicit csp: CSPView): Set[IVar]

  def subconstraints(implicit csp: CSPView): Iterable[Constraint] = Nil

  def onPost(implicit csp: CSPView): Seq[OnPostChange] =
    subconstraints.map(c => Watch(c)).toSeq

  def onWatch(implicit csp: CSPView): Seq[OnWatchChange] =
    subconstraints.map(c => Watch(c)).toSeq

  def propagate(event: Event)(implicit csp: CSPView): PropagationResult

  def satisfaction(implicit csp: CSPView): Satisfaction

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  def reverse: Constraint

  final def isSatisfied(implicit csp: CSPView) = satisfaction == ConstraintSatisfaction.SATISFIED

  final def isViolated(implicit csp: CSPView) = satisfaction == ConstraintSatisfaction.VIOLATED

  final def active(implicit csp: CSP): Boolean  = csp.constraints.isActive(this)
  final def watched(implicit csp: CSP): Boolean = csp.constraints.isWatched(this)

  def &&(constraint: Constraint): ConjunctionConstraint = (this, constraint) match {
    case (c1: ConjunctionConstraint, c2: ConjunctionConstraint) =>
      new ConjunctionConstraint(c1.constraints ++ c2.constraints)
    case (c1: ConjunctionConstraint, c2) =>
      new ConjunctionConstraint(c1.constraints :+ c2)
    case (c1, c2: ConjunctionConstraint) =>
      new ConjunctionConstraint(c1 :: c2.constraints.toList)
    case (c1, c2) =>
      new ConjunctionConstraint(List(c1, c2))
  }

  def ||(constraint: Constraint) = (this, constraint) match {
    case (c1: DisjunctiveConstraint, c2: DisjunctiveConstraint) =>
      new DisjunctiveConstraint(c1.disjuncts ++ c2.disjuncts)
    case (c1: DisjunctiveConstraint, c2) =>
      new DisjunctiveConstraint(c1.disjuncts :+ c2)
    case (c1, c2: DisjunctiveConstraint) =>
      new DisjunctiveConstraint(c1 :: c2.disjuncts.toList)
    case (c1, c2) =>
      new DisjunctiveConstraint(List(c1, c2))
  }
}

class ConstraintSatisfaction

object ConstraintSatisfaction {
  object SATISFIED extends ConstraintSatisfaction
  object VIOLATED  extends ConstraintSatisfaction
  object UNDEFINED extends ConstraintSatisfaction
}
