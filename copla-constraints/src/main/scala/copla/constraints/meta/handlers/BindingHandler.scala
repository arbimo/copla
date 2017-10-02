package copla.constraints.meta.handlers

import copla.constraints.meta.{CSP, updates}
import copla.constraints.meta.constraints.BindConstraint
import copla.constraints.meta.events.{Event, InternalCSPEventHandler, NewConstraint}
import copla.constraints.meta.updates.Update
import copla.constraints.meta.variables.{IVar, IntVariable}

import scala.collection.mutable


class BindingHandler private (val boundVariables: mutable.Set[IntVariable]) extends InternalCSPEventHandler {

  def this() = this(mutable.Set())

  def isBound(variable: IntVariable): Boolean = boundVariables.contains(variable)

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler = new BindingHandler(boundVariables.clone())

  override def handleEvent(event: Event): Update = event match {
    case NewConstraint(c: BindConstraint) =>
      boundVariables += c.variable
      updates.consistent
    case _ =>
      updates.consistent
  }
}
