package copla.planning.events

import copla.lang.model.core
import copla.planning.causality.support.SupportVar

case class ActionInsertion(action: core.ActionTemplate, supportFor: Option[SupportVar]) extends PlanningEvent
