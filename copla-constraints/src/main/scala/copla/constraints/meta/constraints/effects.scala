package copla.constraints.meta.constraints

import copla.constraints.meta.CSP
import copla.constraints.meta.decisions.Decision
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.stn.constraint.TemporalConstraint
import copla.constraints.meta.variables.IntVariable

sealed trait Change
sealed trait OnPostChange        extends Change
sealed trait OnWatchChange       extends Change
sealed trait OnPropagationChange extends Change

case class UpdateDomain(variable: IntVariable, domain: Domain) extends OnPropagationChange
case class Watch(constraint: Constraint)
    extends OnPostChange
    with OnWatchChange
    with OnPropagationChange
case class Post(constraint: Constraint)                  extends OnPostChange with OnPropagationChange
case class DelegateToStn(constraint: TemporalConstraint) extends OnPostChange

case class InitData[T <: ConstraintData](constraint: Constraint with WithData[T], field: T)
    extends OnPostChange
case class UpdateData[T <: ConstraintData](constraint: Constraint with WithData[T],
                                           update: T => Unit)
    extends OnPropagationChange
    with OnPostChange

case class AddDecision(dec: Decision) extends OnPostChange
case class RetractDecision(dec: Decision) extends OnPropagationChange

/** Represent the output of a propagation process. This includes,
  * (i) a set of updates to the CSP induced by the propagation and
  * (ii) the status of the constraint after those effects are propagated.
  */
sealed trait PropagationResult

object PropagationResult {
  def from(satisfaction: ConstraintSatisfaction) = satisfaction match {
    case ConstraintSatisfaction.SATISFIED => Satisfied()
    case ConstraintSatisfaction.VIOLATED  => Inconsistency
    case _ => Undefined()
  }
}

/** Denotes that the constraint induces an inconsistency in the network. */
object Inconsistency extends PropagationResult

/** Denotes that:
  * (i) the constraint induces the provided changes to the CSP,
  * (ii) once the changes are applied, the constraint will be satisfied.
  * @param changes Changes to be applied to the CSP
  */
final case class Satisfied(changes: Seq[OnPropagationChange]) extends PropagationResult

object Satisfied {
  private val emptyInstance: Satisfied                        = new Satisfied(Nil)
  def apply(): Satisfied                                      = emptyInstance
  def apply(change: OnPropagationChange)                      = new Satisfied(change :: Nil)
  def apply(c1: OnPropagationChange, c2: OnPropagationChange) = new Satisfied(c1 :: c2 :: Nil)
}

/** Denotes that:
  * - the constraint induces the provided changes top the CSP
  * - these changes are not sufficient to make the constraint satisfied or violated
  * @param changes Changes to be applied to the CSP.
  */
final case class Undefined(changes: Seq[OnPropagationChange]) extends PropagationResult

object Undefined {
  private val emptyInstance: Undefined                        = new Undefined(Nil)
  def apply(): Undefined                                      = emptyInstance
  def apply(change: OnPropagationChange)                      = new Undefined(change :: Nil)
  def apply(c1: OnPropagationChange, c2: OnPropagationChange) = new Undefined(c1 :: c2 :: Nil)
}
