package copla.constraints.meta.stn.constraint

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.ConstraintSatisfaction
import copla.constraints.meta.stn.variables.Timepoint
import copla.constraints.meta.variables.IVar

class AbsoluteBeforeConstraint(val tp: Timepoint, val deadline: Int) extends TemporalConstraint {

  override def satisfaction(implicit csp: CSPView): Satisfaction =
    if (tp.domain.ub <= deadline)
      ConstraintSatisfaction.SATISFIED
    else if (tp.domain.lb > deadline)
      ConstraintSatisfaction.VIOLATED
    else
      ConstraintSatisfaction.UNDEFINED

  override def variables(implicit csp: CSPView): Set[IVar] =
    Set(csp.varStore.getDelayVariable(csp.temporalOrigin, tp))

  override def toString = s"$tp <= $deadline"

  override def reverse: AbsoluteAfterConstraint = tp > deadline
}
