package copla.planning.structures

import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.stn.variables.TemporalInterval
import copla.lang.model.core
import copla.planning.events.PlanningHandler
import copla.planning.variables.{SVar, Var}

sealed trait PStruct

case class PlanningConstraint(constraint: Constraint) extends PStruct

sealed trait CausalStruct extends PStruct
object CausalStruct {
  type AssertionWithChange = core.TimedAssertion with core.ProvidesChange

  type AssertionNeedingSupport = core.TimedAssertion with core.RequiresSupport

  def assertionAsPlanningStructs(assertion: core.TimedAssertion,
                                 p: PlanningHandler): Seq[PStruct] = {
    assertion match {
      case statement: core.TimedEqualAssertion =>
        val hold = new Holds(p.sv(statement.fluent),
                             p.variable(statement.value),
                             new TemporalInterval(p.tp(statement.start), p.tp(statement.end)),
                             precedingChange = false,
                             statement)
        Seq(hold)
      case statement: core.TimedTransitionAssertion =>
        val hold = new Holds(p.sv(statement.fluent),
                             p.variable(statement.from),
                             new TemporalInterval(p.tp(statement.start), p.tp(statement.start)),
                             precedingChange = true,
                             statement)
        val change = new Change(
          p.sv(statement.fluent),
          p.variable(statement.to),
          new TemporalInterval(p.tp(statement.start), p.tp(statement.end)),
          new TemporalInterval(p.tp(statement.end), p.csp.varStore.getTimepoint()),
          statement
        )
        Seq(hold, change)

      case statement: core.TimedAssignmentAssertion =>
        val endPersist = p.csp.varStore.getTimepoint()
        val change = new Change(
          p.sv(statement.fluent),
          p.variable(statement.value),
          new TemporalInterval(p.tp(statement.start - 1), p.tp(statement.start)),
          new TemporalInterval(p.tp(statement.start), endPersist),
          statement
        )
        Seq(change, PlanningConstraint(p.tp(statement.end) <= endPersist))
    }

  }

}

/**
  *
  * @param fluent Fluent whose value will change
  * @param value Value the fluent is going to take
  * @param changing Temporal interval over which the value is changing
  * @param persists Temporal interval over which the new value CAN persist.
  * @param ref Assertion that lead to the creation of this structure.
  */
class Change(val fluent: SVar,
             val value: Var,
             val changing: TemporalInterval,
             val persists: TemporalInterval,
             val ref: CausalStruct.AssertionWithChange)
    extends CausalStruct {

  override def toString = s"$fluent := $value"
}

/** Assertion requiring the state variable `sv` to have the value `value` over the (inclusive) temporal interval `persists`.
  * If preceding change is true, then the state variable will start changing value at `persists.end +1`.
  * This is typically the case when it represents the initial condition of a transition.*/
class Holds(val fluent: SVar,
            val value: Var,
            val persists: TemporalInterval,
            val precedingChange: Boolean,
            val ref: CausalStruct.AssertionNeedingSupport)
    extends CausalStruct {

  override def toString = s"$fluent == $value"
}

object Holds {

  def apply(statement: CausalStruct.AssertionNeedingSupport, p: PlanningHandler): Holds = {
    statement match {
      case statement: core.TimedEqualAssertion =>
        new Holds(p.sv(statement.fluent),
                  p.variable(statement.value),
                  new TemporalInterval(p.tp(statement.start), p.tp(statement.end)),
                  precedingChange = false,
                  statement)
      case statement: core.TimedTransitionAssertion =>
        new Holds(p.sv(statement.fluent),
                  p.variable(statement.from),
                  new TemporalInterval(p.tp(statement.start), p.tp(statement.start)),
                  precedingChange = true,
                  statement)
    }
  }
}
