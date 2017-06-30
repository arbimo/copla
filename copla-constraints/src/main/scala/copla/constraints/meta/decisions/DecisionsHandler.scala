package copla.constraints.meta.decisions

import copla.constraints.meta.CSP
import copla.constraints.meta.events.{Event, InternalCSPEventHandler}
import copla.constraints.meta.events.{Event, NewVariableEvent}
import copla.constraints.meta.variables.VarWithDomain

import scala.collection.mutable

final class DecisionsHandler(_csp: CSP, base: Option[DecisionsHandler] = None)
    extends InternalCSPEventHandler {

  private val pendingDecisions: mutable.ArrayBuffer[Decision] = base match {
    case Some(prev) => prev.pendingDecisions.clone()
    case None       => mutable.ArrayBuffer()
  }

  override def handleEvent(event: Event) {
    event match {
      case NewVariableEvent(v: VarWithDomain) if v.isDecisionVar =>
        add(new VarBinaryDecision(v))

      case _ =>
    }
  }

  def add(decision: Decision) {
    pendingDecisions += decision
  }

  def pending: Seq[Decision] = pendingDecisions

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new DecisionsHandler(newCSP, Some(this))
}
