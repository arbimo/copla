package copla.constraints.meta.stn.variables

import copla.constraints.meta.CSPView
import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.domains.IntervalDomain
import copla.constraints.meta.stn.constraint.{AbsoluteAfterConstraint, AbsoluteBeforeConstraint, MinDelay}
import copla.constraints.meta.variables.VarWithDomain

case class RelativeTimepoint(tp: Timepoint, delay: Int) extends VarWithDomain {

  override def domain(implicit csp: CSPView): IntervalDomain = tp.domain.shift(delay)

  def +(delay: Int): RelativeTimepoint = RelativeTimepoint(tp, this.delay + delay)
  def -(delay: Int): RelativeTimepoint = RelativeTimepoint(tp, this.delay - delay)

  def -(o: RelativeTimepoint): TemporalDelay = new TemporalDelay(o, this)

  def <=(o: RelativeTimepoint): MinDelay = new MinDelay(tp, o.tp, delay - o.delay)
  def <(o: RelativeTimepoint): MinDelay  = new MinDelay(tp, o.tp, delay - o.delay + 1)

  def >=(o: RelativeTimepoint): MinDelay = o <= this
  def >(o: RelativeTimepoint): MinDelay  = o < this

  def <=(deadline: Int): AbsoluteBeforeConstraint =
    new AbsoluteBeforeConstraint(tp, deadline - delay)

  def <(deadline: Int): AbsoluteBeforeConstraint =
    new AbsoluteBeforeConstraint(tp, deadline - delay - 1)

  def >=(deadline: Int): AbsoluteAfterConstraint =
    new AbsoluteAfterConstraint(tp, deadline - delay)

  def >(deadline: Int): AbsoluteAfterConstraint =
    new AbsoluteAfterConstraint(tp, deadline - delay + 1)

  def ===(o: RelativeTimepoint): Constraint = this >= o && this <= o
  def =!=(o: RelativeTimepoint): Constraint = this < o || this > o
  def ===(deadline: Int): Constraint        = this <= deadline && this >= deadline
  def =!=(deadline: Int): Constraint        = this < deadline || this > deadline

  override def toString: String =
    if (delay == 0) tp.toString
    else if (delay > 0) s"$tp + $delay"
    else s"$tp - ${-delay}"

  /** If true, a new decision will be generated when the variable is added to a CSP. */
  override def isDecisionVar: Boolean = false
}
