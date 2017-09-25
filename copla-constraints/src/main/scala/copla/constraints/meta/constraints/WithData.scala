package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}

/** A constraint with a data field maintained in the CSP instance.
  *
  * The main use is to keep mutable data structures since a Constraint should be Immutable and remain independent
  * of any particular CSP instances.
  *
  * This interface defines convenience function to store and access data fields in a CSP.
  * */
trait WithData[DataType <: ConstraintData] extends Constraint {

  def data(implicit csp: CSPView): DataType =
    csp.constraints.dataOf(this)

  def hasData(implicit csp: CSPView): Boolean =
    csp.constraints.hasDataOf(this)

  @deprecated("Should use the dedicated Change interface to update from propagation results.")
  def setData(d: DataType)(implicit csp: CSP) {
    csp.constraints.setDataOf(this, d)
  }

}
