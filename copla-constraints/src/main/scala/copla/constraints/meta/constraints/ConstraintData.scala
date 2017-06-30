package copla.constraints.meta.constraints

import copla.constraints.meta.CSP

/** A data field that is to be stored inside a CSP.
  * This interface is used to provide the capability for a CSP to replicate a data field when being cloned.
  * */
trait ConstraintData {

  def clone(implicit newCSP: CSP): ConstraintData

}
