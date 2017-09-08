package copla.planning.structures

import copla.anml.model.concrete.statements.{LogStatement, Persistence, Transition}
import copla.constraints.meta.stn.variables.TemporalInterval
import copla.planning.events.PlanningHandler
import copla.planning.variables.{SVar, Var}

/** Assertion requiring the state variable `sv` to have the value `value` over the (inclusive) temporal interval `persists`.
  * If preceding change is true, then the state variable will start changing value at `persists.end +1`.
  * This is typically the case when it represents the initial condition of a transition.*/
class Holds(val sv: SVar, val value: Var, val persists: TemporalInterval, val precedingChange: Boolean, val ref: LogStatement)
  extends CausalStruct {
  assert(ref.needsSupport)

  override def toString = s"$sv == $value"
}

object Holds {

  def apply(statement: LogStatement, p: PlanningHandler): Holds = {
    statement match {
      case statement: Persistence =>
        new Holds(
          p.sv(statement.sv),
          p.variable(statement.value),
          new TemporalInterval(p.tp(statement.start), p.tp(statement.end)),
          precedingChange = false,
          statement)
      case statement: Transition =>
        new Holds(
          p.sv(statement.sv),
          p.variable(statement.startValue),
          new TemporalInterval(p.tp(statement.start), p.tp(statement.start)),
          precedingChange = true,
          statement)
    }
  }
}
