package copla.constraints.meta.search.heuristics

import copla.constraints.meta.events.InternalCSPEventHandler

/** Defines a heuristic to be attached to the CSP. The heuristic will receive all internal events of the CSP. */
trait Heuristic extends InternalCSPEventHandler {

  /** Gives the priority of a given CSP. Lower value means higher priority. */
  def priority: Float
}
