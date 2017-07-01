package copla.constraints.meta.stn.constraint

import copla.constraints.meta.constraints.ConstraintSatisfaction.{SATISFIED, UNDEFINED, VIOLATED}
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints._
import copla.constraints.meta.events.Event
import copla.constraints.meta.events.NewConstraint
import copla.constraints.meta.stn.variables.Timepoint
import copla.constraints.meta.variables.IVar

abstract class TemporalConstraint extends Constraint {

  override def onPost(implicit csp: CSPView): Seq[OnPostChange] = {
    super.onPost :+ DelegateToStn(this)
  }

  override def propagate(event: Event)(implicit csp: CSPView) = {
    // propagation handled by the STN, simply return constraint status
    PropagationResult.from(satisfaction)
  }
}

class MinDelay(val src: Timepoint, val dst: Timepoint, val minDelay: Int)
    extends TemporalConstraint {
  override def toString = s"$src + $minDelay <= $dst"

  override def variables(implicit csp: CSPView): Set[IVar] =
    Set(csp.varStore.getDelayVariable(src, dst))

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val dom = csp.varStore.getDelayVariable(src, dst).domain
    if (dom.lb >= minDelay)
      ConstraintSatisfaction.SATISFIED
    else if (dom.ub < minDelay)
      ConstraintSatisfaction.VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED
  }

  override def reverse: MinDelay =
    new MinDelay(dst, src, -minDelay + 1)
}

class Contingent(val src: Timepoint, val dst: Timepoint, val min: Int, val max: Int)
    extends TemporalConstraint {

  override def variables(implicit csp: CSPView): Set[IVar] = Set(src, dst)

  override def toString = s"$src == [$min, $max] ==> $dst"

  override def satisfaction(implicit csp: CSPView): Satisfaction = ConstraintSatisfaction.UNDEFINED

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = ???
}
