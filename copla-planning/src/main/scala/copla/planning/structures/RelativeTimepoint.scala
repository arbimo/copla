package copla.planning.structures

import copla.constraints.meta.stn.constraint.MinDelay
import copla.constraints.meta.stn.variables.Timepoint
import copla.lang.model.core

import scala.concurrent.duration.Deadline


class RelativeTimepoint(tp: Timepoint, delay: Int) {

  def +(delay: Int): RelativeTimepoint = new RelativeTimepoint(tp, this.delay + delay)

  def <=(o: RelativeTimepoint): MinDelay = new MinDelay(tp, o.tp, delay - o.delay)
  def <(o: RelativeTimepoint): MinDelay = new MinDelay(tp, o.tp, delay - o.delay +1)

  def >=(o: RelativeTimepoint): MinDelay = o <= this
  def >(o: RelativeTimepoint): MinDelay = o < this

  def <=(deadline: Int) = tp <= (deadline - delay)
  def <(deadline: Int) = tp < (deadline - delay)
  def >=(deadline: Int) = tp >= (deadline - delay)
  def >(deadline: Int) = tp > (deadline - delay)

  def ===(o: RelativeTimepoint) = this >= o && this <= o
  def =!=(o: RelativeTimepoint) = this < o || this > o
  def ===(deadline: Int) = this <= deadline && this >= deadline
  def =!=(deadline: Int) = this < deadline || this > deadline

  override def toString: String =
    if(delay == 0) tp.toString
    else if(delay > 0) s"$tp + $delay"
    else s"$tp - ${-delay}"

}
