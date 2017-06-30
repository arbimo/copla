package copla.constraints.meta.decisions

import copla.constraints.meta.CSP

trait DecisionOption {

  /** This method should enforce the decision option in the given CSP. */
  def enforceIn(csp: CSP)

}
