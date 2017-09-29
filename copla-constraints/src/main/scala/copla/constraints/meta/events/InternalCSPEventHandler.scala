package copla.constraints.meta.events

import copla.constraints.meta.{CSP, updates}

trait CSPEventHandler {
  def handleEvent(event: Event): updates.Update
}

trait InternalCSPEventHandler extends CSPEventHandler {

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  def clone(newCSP: CSP): InternalCSPEventHandler

}
