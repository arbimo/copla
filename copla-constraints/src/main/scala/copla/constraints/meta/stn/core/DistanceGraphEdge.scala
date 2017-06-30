package copla.constraints.meta.stn.core

import copla.constraints.meta.stn.variables.Timepoint

class DistanceGraphEdge(val from: Timepoint, val to: Timepoint, val value: Int) {
  override def toString = s"(min-delay $to $from ${-value})"
}
