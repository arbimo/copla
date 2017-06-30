package copla.constraints.meta.stn.variables

import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.domains.IntervalDomain
import copla.constraints.meta.stn.constraint.{AbsoluteAfterConstraint, AbsoluteBeforeConstraint}
import copla.constraints.meta.stn.constraint.{AbsoluteBeforeConstraint, MinDelay}
import copla.constraints.meta.variables.{IVar, VarWithDomain}

class Timepoint(val id: Int, ref: Option[Any]) extends VarWithDomain {

  override def domain(implicit csp: CSP): IntervalDomain = csp.dom(this)

  def isStructural: Boolean = false
  def isContingent: Boolean = false

  def <(tp: Timepoint): MinDelay =
    new MinDelay(this, tp, 1)

  def <=(tp: Timepoint): MinDelay =
    new MinDelay(this, tp, 0)

  def >(tp: Timepoint): MinDelay =
    tp < this

  def >=(tp: Timepoint): MinDelay =
    tp <= this

  def <=(deadline: Int): AbsoluteBeforeConstraint = {
    new AbsoluteBeforeConstraint(this, deadline)
  }

  def <(deadline: Int): AbsoluteBeforeConstraint = {
    new AbsoluteBeforeConstraint(this, deadline - 1)
  }

  def >=(deadline: Int): AbsoluteAfterConstraint = {
    new AbsoluteAfterConstraint(this, deadline)
  }

  def >(deadline: Int): AbsoluteAfterConstraint = {
    new AbsoluteAfterConstraint(this, deadline + 1)
  }

  def ===(other: Timepoint) = this <= other && this >= other

  def =!=(other: Timepoint): Constraint = this < other || this > other

  override def ===(value: Int) = this <= value && this >= value

  override def =!=(value: Int): Constraint = this < value || this > value

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
