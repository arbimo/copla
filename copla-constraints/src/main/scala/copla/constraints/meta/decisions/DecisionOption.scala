package copla.constraints.meta.decisions

import copla.constraints.meta.{CSP, CSPView}

trait DecisionOption {

  /** This method should enforce the decision option in the given CSP. */
  def enforceIn(csp: CSP) // TODO: shouldn't this return a CSPUpdateResult

  def negate(implicit csp: CSPView): DecisionOption

}
