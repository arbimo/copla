package copla.planning.causality.support

import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.{Constraint, ConstraintData}
import copla.constraints.meta.util.Assertion._

import scala.collection.mutable

/** Data fields to be accessed by a SupportConstraint.
  * This is a bidirectional mapping from integers in the domain of the decision variable
  * to the constraints they represent. */
class SupportConstraintData(base: Option[SupportConstraintData] = None) extends ConstraintData {

  private val constraintsByDomainValue: scala.collection.mutable.Map[Int, Constraint] = base match {
    case Some(prev) => prev.constraintsByDomainValue.clone()
    case None       => mutable.Map()
  }
  private val domainValueByConstraint: scala.collection.mutable.Map[Constraint, Int] = base match {
    case Some(prev) => prev.domainValueByConstraint.clone()
    case None       => mutable.Map()
  }

  def put(domainValue: Int, subConstraint: Constraint) {
    assert1(!constraintsByDomainValue.contains(domainValue))
    constraintsByDomainValue.put(domainValue, subConstraint)
    domainValueByConstraint.put(subConstraint, domainValue)
  }

  def hasConstraintFor(domainValue: Int): Boolean = constraintsByDomainValue.contains(domainValue)

  def constraintOf(domainValue: Int): Constraint = constraintsByDomainValue(domainValue)

  def indexOf(constraint: Constraint): Int = domainValueByConstraint(constraint)

  def clone(implicit context: CSP) = new SupportConstraintData(Some(this))
}
