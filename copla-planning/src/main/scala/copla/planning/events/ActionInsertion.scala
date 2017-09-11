package copla.planning.events

import copla.planning.causality.support.SupportVar

case class ActionInsertion(action: Any /* TODO: AbstractAction */, supportFor: Option[SupportVar]) extends PlanningEvent
