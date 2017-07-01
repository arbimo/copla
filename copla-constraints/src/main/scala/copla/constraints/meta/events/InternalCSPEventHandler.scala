package copla.constraints.meta.events

import copla.constraints.meta.{CSP, CSPUpdateResult}

trait CSPEventHandler {
  def handleEvent(event: Event): CSPUpdateResult
}

trait InternalCSPEventHandler extends CSPEventHandler {

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  def clone(newCSP: CSP): InternalCSPEventHandler

}
