package copla.constraints.meta.stn.variables

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.domains.IntervalDomain
import copla.constraints.meta.stn.constraint.{AbsoluteAfterConstraint, AbsoluteBeforeConstraint}
import copla.constraints.meta.stn.constraint.{AbsoluteBeforeConstraint, MinDelay}
import copla.constraints.meta.variables.{IVar, VarWithDomain}

class Timepoint(val id: Int, ref: Option[Any]) extends VarWithDomain {

  override def domain(implicit csp: CSPView): IntervalDomain = csp.dom(this)

  def isStructural: Boolean = false
  def isContingent: Boolean = false

  def +(delay: Int): RelativeTimepoint = RelativeTimepoint(this, delay)
  def -(delay: Int): RelativeTimepoint = RelativeTimepoint(this, -delay)
  def -(o: RelativeTimepoint): TemporalDelay = Timepoint.asRelativeTimepoint(this) - o
  def -(o: Timepoint): TemporalDelay = this - Timepoint.asRelativeTimepoint(o)

  override def ===(value: Int): Constraint = Timepoint.asRelativeTimepoint(this) === value

  override def =!=(value: Int): Constraint = Timepoint.asRelativeTimepoint(this) =!= value

  override def toString = ref match {
    case Some(x) => s"$x($id)"
    case None    => s"tp$id"
  }

  override final def hashCode: Int = id
  override final def equals(o: Any): Boolean = o match {
    case o: Timepoint => id == o.id
    case _            => false
  }

  /** Not a decision variable by default */
  override def isDecisionVar: Boolean = false
}
object Timepoint {

  implicit def asRelativeTimepoint(tp: Timepoint): RelativeTimepoint = new RelativeTimepoint(tp, 0)
}