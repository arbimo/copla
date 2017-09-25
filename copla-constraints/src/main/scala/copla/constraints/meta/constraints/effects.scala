package copla.constraints.meta.constraints

import copla.constraints.meta.domains.Domain
import copla.constraints.meta.stn.constraint.TemporalConstraint
import copla.constraints.meta.variables.IntVariable

sealed trait Change
sealed trait OnPostChange        extends Change
sealed trait OnWatchChange       extends Change
sealed trait OnPropagationChange extends Change

case class UpdateDomain(variable: IntVariable, domain: Domain) extends OnPropagationChange
case class Watch(constraint: Constraint)                       extends OnPostChange with OnWatchChange
case class Post(constraint: Constraint)                        extends OnPostChange with OnPropagationChange
case class DelegateToStn(constraint: TemporalConstraint)       extends OnPostChange

/** Represent the output of a propagation process. This includes,
  * (i) a set of updates to the CSP induced by the propagation and
  * (ii) the status of the constraint after those effects are propagated.
  */
sealed trait PropagationResult

object PropagationResult {
  def from(satisfaction: ConstraintSatisfaction) = satisfaction match {
    case ConstraintSatisfaction.SATISFIED => Satisfied()
    case ConstraintSatisfaction.UNDEFINED => Undefined()
    case ConstraintSatisfaction.VIOLATED  => Inconsistency
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
  def apply()                                                 = new Satisfied(Nil)
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
  def apply()                                                 = new Undefined(Nil)
  def apply(change: OnPropagationChange)                      = new Undefined(change :: Nil)
  def apply(c1: OnPropagationChange, c2: OnPropagationChange) = new Undefined(c1 :: c2 :: Nil)
}
