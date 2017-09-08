package copla.planning.events

import copla.constraints.meta.events.CSPEventHandler

trait PlanningEventHandler extends CSPEventHandler {

  def clone(newContext: PlanningHandler) : PlanningEventHandler

  def report : String
}
