package copla.planning.events

import copla.anml.model.concrete.Chronicle
import copla.constraints.meta.events.Event
import copla.planning.structures.PStruct

trait PlanningEvent extends Event

object InitPlanner extends PlanningEvent

case class ChronicleAdded(chronicle: Chronicle) extends PlanningEvent

case class PlanningStructureAdded(struct: PStruct) extends PlanningEvent