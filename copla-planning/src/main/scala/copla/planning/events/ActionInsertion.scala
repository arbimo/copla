package copla.planning.events

import copla.anml.model.abs.AbstractAction
import copla.planning.causality.support.SupportVar

case class ActionInsertion(action: AbstractAction, supportFor: Option[SupportVar]) extends PlanningEvent
