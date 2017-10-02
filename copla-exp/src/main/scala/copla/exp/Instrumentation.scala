package copla.exp

import copla.constraints.meta.events.{CSPEventHandler, Event, InternalCSPEventHandler}
import copla.constraints.meta.{CSP, updates}
import copla.constraints.meta.updates.Update

import scala.collection.mutable

class Instrumentation(val eventsCount: mutable.Map[String, Int] = mutable.Map()) extends InternalCSPEventHandler {

  override def handleEvent(event: Event): Update = {
    val name = event.getClass.getSimpleName

    eventsCount(name) = eventsCount.getOrElse(name, 0) + 1
    updates.consistent
  }

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new Instrumentation(eventsCount.clone())
}
