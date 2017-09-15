package copla.constraints.meta.stn.variables

import copla.constraints.meta.CSP

class TemporalInterval(val start: RelativeTimepoint, val end: RelativeTimepoint) {

  override def toString = s"[$start, $end]"

  def duration(implicit csp: CSP): TemporalDelay =
    csp.varStore.getDelayVariable(start, end)

  def <(o: TemporalInterval)  = this.end < o.start
  def <=(o: TemporalInterval) = this.end <= o.start
  def >(o: TemporalInterval)  = o < this
  def >=(o: TemporalInterval) = o <= this
}
