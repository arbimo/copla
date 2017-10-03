package copla.constraints.meta.constraints.specialization

import copla.constraints.meta.CSPView
import copla.constraints.meta.constraints._
import copla.constraints.meta.events.Event
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.IVar

/**
  * Denotes a CSP specific view of a constraint.
  * The constraint will pretend to be the original constraint through its hashCode and equals methods) but
  *   delegates propagation to the specialized constraint.
  * @param specialized A constraint simplified for a particular CSP instance.
  * @param general The original constraint.
  */
class Specialization private (val specialized: Constraint, val general: Constraint) extends Constraint {
  override def variables(implicit csp: CSPView): Set[IVar] =
    specialized.variables

  override def propagate(event: Event)(implicit csp: CSPView): PropagationResult =
    specialized.propagate(event)

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    assert3(general.isUndefined || specialized.satisfaction == general.satisfaction)
    specialized.satisfaction
  }

  override def subconstraints(implicit csp: CSPView): Iterable[Constraint] = specialized.subconstraints

  override def onPost(implicit csp: CSPView): Seq[OnPostChange] = specialized.onPost

  override def onWatch(implicit csp: CSPView): Seq[OnWatchChange] = specialized.onWatch

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint = specialized.reverse

  override def toString: String = s"$general <- spec[$specialized]"
}

object Specialization {

  /** Builds a specialized view of this constraint for the given CSP.
    * A specialized constraint typically omits subconstraints that are either violated or satisfied in the given CSP. */
  protected[meta] def apply(toSpecialize: Constraint)(implicit csp: CSPView): Constraint =
    specialize(toSpecialize)

  private def tautology(c: Constraint) = new Specialization(Tautology(), c)
  private def contradiction(c: Constraint) = new Specialization(Contradiction(), c)
  private def spec(spec: Constraint, gen: Constraint) = new Specialization(spec, gen)

  private def specialize(constraint: Constraint)(implicit csp: CSPView): Constraint = {
    constraint match {
      case c: EqualityConstraint if c.v1 == c.v2 =>
        tautology(c)

      case c: InequalityConstraint if c.v1 == c.v2 =>
        contradiction(c)

      case c: DisjunctiveConstraint =>
        val disjuncts = c.disjuncts.map(sc => specialize(sc)).filterNot(_.isViolated)
        if(disjuncts.exists(_.isSatisfied))
          tautology(c)
        else if(disjuncts.isEmpty)
          contradiction(c)
        else
          spec(new DisjunctiveConstraint(disjuncts), c)

      case c: ConjunctionConstraint =>
        val conjuncts = c.constraints.map(specialize).filterNot(_.isSatisfied)
        if(conjuncts.isEmpty)
          tautology(c)
        else if(conjuncts.exists(_.isViolated))
          contradiction(c)
        else
          spec(new ConjunctionConstraint(conjuncts), c)

      case c =>
        c
    }
  }

}
