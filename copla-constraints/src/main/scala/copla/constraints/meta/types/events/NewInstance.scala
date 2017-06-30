package copla.constraints.meta.types.events

import copla.constraints.meta.events.CSPEvent
import copla.constraints.meta.types.dynamics.DynamicType

case class NewInstance[T](dynType: DynamicType[T], instance: T, value: Int) extends CSPEvent {
  require(!dynType.isStatic)
  require(dynType.subTypes.isEmpty)
}
