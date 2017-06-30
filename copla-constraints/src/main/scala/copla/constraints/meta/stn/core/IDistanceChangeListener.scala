package copla.constraints.meta.stn.core

import copla.constraints.meta.stn.variables.Timepoint

trait IDistanceChangeListener {

  def distanceUpdated(tp1: Timepoint, tp2: Timepoint)

}
