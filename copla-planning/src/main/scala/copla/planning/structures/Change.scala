package copla.planning.structures

import copla.anml.model.concrete.statements.LogStatement
import copla.constraints.meta.stn.variables.TemporalInterval
import copla.planning.events.PlanningHandler
import copla.planning.variables.{SVar, Var}

class Change(val sv: SVar, val value: Var, val changing: TemporalInterval, val persists: TemporalInterval, val ref: LogStatement)
  extends CausalStruct {
  assert(ref.isChange)

  override def toString = s"$sv := $value"
}

object Change {

  def apply(statement: LogStatement, p: PlanningHandler) : Change = {
    require(statement.isChange)
    new Change(
      p.sv(statement.sv),
      p.variable(statement.endValue),
      new TemporalInterval(p.tp(statement.start), p.tp(statement.end)),
      new TemporalInterval(p.tp(statement.end), p.csp.varStore.getTimepoint()),
      statement)
  }

}
