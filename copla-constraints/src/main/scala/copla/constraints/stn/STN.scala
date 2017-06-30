package copla.constraints.stn

trait STN {

  /** Records a new time point in the STN. Returns its ID (which might change) */
  def recordTimePoint(tp: TPRef): Int

  /** Record this time point as the global start of the STN */
  def recordTimePointAsStart(tp: TPRef): Int

  /** Unifies this time point with the global end of the STN */
  def recordTimePointAsEnd(tp: TPRef): Int

  def getStartTimePoint: Option[TPRef]

  def getEndTimePoint: Option[TPRef]

  /** Remove a timepoint and all associated constraints from the STN */
  def removeTimePoint(tp: TPRef)

  /** Set the distance from the global start of the STN to tp to time */
  def setTime(tp: TPRef, time: Int)

  /** Enforce u <= v */
  final def enforceBefore(u: TPRef, v: TPRef) { enforceMinDelay(u, v, 0) }

  /** Enforces u < v */
  final def enforceStrictlyBefore(u: TPRef, v: TPRef) { enforceMinDelay(u, v, 1) }

  /** Enforces u +d <= v */
  final def enforceMinDelay(u: TPRef, v: TPRef, d: Int) { addConstraint(v, u, -d) }

  /** Enforces u + d >= v */
  final def enforceMaxDelay(u: TPRef, v: TPRef, d: Int) { addConstraint(u, v, d) }

  protected def addConstraint(u: TPRef, v: TPRef, w: Int)

  /** Enforce v in [u+min, u+max] */
  final def enforceConstraint(u: TPRef, v: TPRef, min: Int, max: Int): Boolean = {
    enforceMinDelay(u, v, min)
    enforceMaxDelay(u, v, max)
    isConsistent()
  }

  /** Returns True if u can be at the same time or before v */
  final def canBeBefore(u: TPRef, v: TPRef): Boolean = isConstraintPossible(v, u, 0)

  /** Returns True if u can be strictly before v */
  final def canBeStrictlyBefore(u: TPRef, v: TPRef): Boolean = isConstraintPossible(v, u, -1)

  final def isDelayPossible(from: TPRef, to: TPRef, delay: Int) =
    isConstraintPossible(to, from, -delay)

  protected def isConstraintPossible(u: TPRef, v: TPRef, w: Int): Boolean

  /** Returns the minimal time from the start of the STN to u */
  def getEarliestTime(u: TPRef): Int

  /** Returns the maximal time from the start of the STN to u */
  def getLatestTime(u: TPRef): Int

  /** Returns true if the STN is consistent (might trigger a propagation */
  def isConsistent(): Boolean

  /** Makes an independent clone of this STN. */
  def deepCopy(): STN

  def toStringRepresentation: String = ???
}
