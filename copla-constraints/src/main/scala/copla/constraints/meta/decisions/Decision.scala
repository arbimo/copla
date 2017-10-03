package copla.constraints.meta.decisions

import copla.constraints.meta.{CSP, CSPView}

trait Decision {

  /** Returns true is this decision is still pending. */
  def pending(implicit csp: CSPView): Boolean

  /** Estimate of the number of options available (typically used for variable ordering). */
  def numOptions(implicit csp: CSPView): Int

  /** Options to handle advance this decision.
    * Note that the decision can still be pending after applying one of the options.
    * A typical set of options for binary search is [var === val, var =!= val]. */
  def options(implicit csp: CSPView): Seq[DecisionOption]
}
