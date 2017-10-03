package copla.exp

import copla.constraints.meta.events.{CSPEventHandler, Event, InternalCSPEventHandler}
import copla.constraints.meta.{updates, CSP}
import copla.constraints.meta.updates.Update

import scala.collection.mutable

class Instrumentation(
    val eventsClassCount: mutable.Map[String, Int] = mutable.Map(),
    val eventsCount: mutable.Map[String, Int] = mutable.Map()
) extends InternalCSPEventHandler {

  override def handleEvent(event: Event): Update = {
    val name = event.getClass.getSimpleName

    eventsClassCount(name) = eventsClassCount.getOrElse(name, 0) + 1
    eventsCount(event.toString) = eventsCount.getOrElse(event.toString, 0) + 1
    updates.consistent
  }

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new Instrumentation(eventsClassCount.clone(), eventsCount.clone())
}
