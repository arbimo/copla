package copla.constraints.meta.constraints

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints.ConstraintSatisfaction._
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events.{DomainReduced, Event, WatchedSatisfied, WatchedViolated}
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables._

class DisjunctiveConstraint(val disjuncts: Seq[Constraint]) extends Constraint {

  private val decisionVar = new IntVar(Domain(disjuncts.indices.toSet)) {
    override def toString: String = s"disjunctive-dec-var[${DisjunctiveConstraint.this}]"
  }

  override def variables(implicit csp: CSPView): Set[IVar] = Set(decisionVar)

  override def subconstraints(implicit csp: CSPView) = disjuncts

  override def satisfaction(implicit csp: CSPView): Satisfaction = {
    val satisfactions = disjuncts.map(_.satisfaction)
    if (satisfactions.contains(SATISFIED))
      SATISFIED
    else if (satisfactions.contains(UNDEFINED))
      UNDEFINED
    else
      VIOLATED
  }

  override def propagate(event: Event)(implicit csp: CSPView) = {
    val dom = decisionVar.domain
    if (dom.isSingleton && disjuncts(dom.head).isSatisfied) {
      Satisfied()
    } else {
      event match {
        case WatchedSatisfied(c) =>
          assert3(c.isSatisfied)
          assert3(decisionVar.domain.contains(disjuncts.indexOf(c)),
                  "Decision var does not contain a satisfied constraint")
          val id = disjuncts.indexOf(c)
          Satisfied(
            UpdateDomain(decisionVar, Domain(id))
          )
        case WatchedViolated(c) =>
          assert3(c.isViolated)
          val id = disjuncts.indexOf(c)
          if (decisionVar.boundTo(id)) {
            Inconsistency
          } else if (dom.contains(id)) {
            Undefined(UpdateDomain(decisionVar, dom - id))
          } else {
            Undefined()
          }
        case DomainReduced(v) =>
          assert3(v == decisionVar)
          if (dom.isEmpty) {
            Inconsistency
          } else if (dom.isSingleton) {
            Undefined(Post(disjuncts(dom.head)))
          } else {
            Undefined()
          }
        case _ => // ignore
          Undefined()
      }
    }
  }

  override def toString = "(" + disjuncts.mkString(" || ") + ")"

  override def ||(c: Constraint) =
    new DisjunctiveConstraint(c :: disjuncts.toList)

  /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
  override def reverse: Constraint =
    new ConjunctionConstraint(disjuncts.map(_.reverse))
}
