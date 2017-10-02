package copla.constraints.meta.constraints

import copla.constraints.meta.constraints.specialization.Specialization
import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.IVar

trait Constraint {

  type Satisfaction = ConstraintSatisfaction

  def variables(implicit csp: CSPView): Set[IVar]

  def subconstraints(implicit csp: CSPView): Iterable[Constraint] = Nil

  def onPost(implicit csp: CSPView): Seq[OnPostChange] =
    subconstraints.map(c => Watch(c)).toSeq

  def onWatch(implicit csp: CSPView): Seq[OnWatchChange] =
    subconstraints.map(c => Watch(c)).toSeq

  def propagate(event: Event)(implicit csp: CSPView): PropagationResult

  def satisfaction(implicit csp: CSPView): Satisfaction

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  def reverse: Constraint

  final def isSatisfied(implicit csp: CSPView): Boolean = satisfaction == ConstraintSatisfaction.SATISFIED

  final def isViolated(implicit csp: CSPView): Boolean = satisfaction == ConstraintSatisfaction.VIOLATED

  final def isUndefined(implicit csp: CSPView): Boolean = satisfaction == ConstraintSatisfaction.UNDEFINED

  final def active(implicit csp: CSP): Boolean  = csp.constraints.isActive(this)
  final def watched(implicit csp: CSP): Boolean = csp.constraints.isWatched(this)

  def &&(constraint: Constraint): ConjunctionConstraint = (this, constraint) match {
    case (c1: ConjunctionConstraint, c2: ConjunctionConstraint) =>
      new ConjunctionConstraint(c1.constraints ++ c2.constraints)
    case (c1: ConjunctionConstraint, c2) =>
      new ConjunctionConstraint(c1.constraints :+ c2)
    case (c1, c2: ConjunctionConstraint) =>
      new ConjunctionConstraint(c1 :: c2.constraints.toList)
    case (c1, c2) =>
      new ConjunctionConstraint(List(c1, c2))
  }

  def ||(constraint: Constraint): DisjunctiveConstraint = (this, constraint) match {
    case (c1: DisjunctiveConstraint, c2: DisjunctiveConstraint) =>
      new DisjunctiveConstraint(c1.disjuncts ++ c2.disjuncts)
    case (c1: DisjunctiveConstraint, c2) =>
      new DisjunctiveConstraint(c1.disjuncts :+ c2)
    case (c1, c2: DisjunctiveConstraint) =>
      new DisjunctiveConstraint(c1 :: c2.disjuncts.toList)
    case (c1, c2) =>
      new DisjunctiveConstraint(List(c1, c2))
  }

  protected def equivalent(c: Constraint): Boolean = super.equals(c)
  protected def hash: Int = super.hashCode()

  /** A specialization aware equality constraint.
    * Equality for general constraints can be overrided with the `equivalent` method. */
  override final def equals(obj: scala.Any): Boolean =
    (this, obj) match {
      case (x: Specialization, y: Specialization) => x.general == y.general
      case (x: Specialization, y) => x.general == y
      case (x, y: Specialization) => x == y.general
      case (_, y: Constraint) => equivalent(y)
      case _ => false
    }

  /** Specialization aware hashCode. Default value can be overrided with the `hash` method. */
  override final def hashCode(): Int = this match {
    case x: Specialization => x.general.hashCode()
    case _ => hash
  }
}

/** Enum like trait to represent the status of a constraint in a given network. */
sealed trait ConstraintSatisfaction

object ConstraintSatisfaction {
  object SATISFIED extends ConstraintSatisfaction {
    override val toString: String = "satisfied"
  }
  object VIOLATED  extends ConstraintSatisfaction {
    override val toString: String = "violated"
  }
  object UNDEFINED extends ConstraintSatisfaction {
    override val toString: String = "undefined"
  }
}
