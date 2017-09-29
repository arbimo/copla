package copla.planning.causality

import copla.constraints.meta.CSP
import copla.constraints.meta.updates._
import copla.constraints.meta.events.Event
import copla.constraints.meta.stn.variables.{RelativeTimepoint, Timepoint}
import copla.constraints.meta.types.dynamics.{BaseDynamicType, ComposedDynamicType}
import copla.constraints.meta.types.statics.BaseType
import copla.constraints.meta.util.Assertion._
import copla.planning.causality.support.SupportConstraint
import copla.planning.events.{PlanningEventHandler, PlanningHandler, PlanningStructureAdded}
import copla.planning.structures._

import scala.collection.mutable

sealed trait SupportOption
case class SupportByExistingChange(c: Change)                  extends SupportOption
case class SupportByActionInsertion(a: ActionPotentialSupport) extends SupportOption

object DecisionPending extends SupportOption {
  override def toString: String = "DecisionPending"
}
object DecisionType extends BaseType[SupportOption]("decision-type", List((DecisionPending, 0)))

class CausalHandler(val context: PlanningHandler, base: Option[CausalHandler] = None)
    extends PlanningEventHandler {

  implicit val csp: CSP = context.csp

  val changes: mutable.ArrayBuffer[Change] = base match {
    case Some(prev) => prev.changes.clone()
    case None       => mutable.ArrayBuffer()
  }

  val holds: mutable.ArrayBuffer[Holds] = base match {
    case Some(prev) => prev.holds.clone()
    case None       => mutable.ArrayBuffer()
  }

  val potentialSupports: PotentialSupport = base match {
    case Some(prev) => prev.potentialSupports.clone(this)
    case None       => new PotentialSupport(this)
  }

  val existingSupportType: BaseDynamicType[SupportByExistingChange] = base match {
    case Some(prev) => prev.existingSupportType
    case None =>
      assert1(changes.isEmpty)
      new BaseDynamicType("existing-change-support", Nil)
  }
  val actionInsertionSupportType: BaseType[SupportByActionInsertion] =
    base match {
      case Some(prev) => prev.actionInsertionSupportType
      case None =>
        new BaseType("action-insertion-support",
                     potentialSupports.actionPotentialSupports.values
                       .map(SupportByActionInsertion)
                       .zipWithIndex
                       .toList
                       .map(p => (p._1, p._2 + 1)))
    }
  val supportType: ComposedDynamicType[SupportOption] = base match {
    case Some(prev) => prev.supportType
    case None =>
      new ComposedDynamicType[SupportOption](
        DecisionType :: actionInsertionSupportType :: existingSupportType :: Nil)
  }

  def report: String = {
    def time(tp: RelativeTimepoint): String = s"$tp:${tp.domain}"
    val sb                                  = new StringBuilder
    sb ++= "-------- State Variables --------\n"
    val changesByFluents: Seq[(String, String)] = changes
      .groupBy(c => c.fluent.format)
      .mapValues(
        changes =>
          changes
            .map(c => (c.persists, c.value))
            .sortBy(_._1.start.tp.domain.lb)
            .map(c => s"[${c._1.start.tp.domain.lb}, ${c._1.end.tp.domain.lb}] ${c._2.dom}")
            .mkString(" -> "))
      .toSeq
      .sorted
    for (timeline <- changesByFluents) {
      sb.append(timeline._1)
      sb.append(":   ")
      sb.append(timeline._2)
      sb.append("\n")
    }
    sb.toString()
  }

  override def handleEvent(event: Event): Update = {
    event match {
      case PlanningStructureAdded(s: Holds) =>
        foreach(holds)(h => csp.post(Threat(s, h))) >>
          foreach(changes)(c => csp.post(Threat(c, s))) >>
          csp.post(new SupportConstraint(supportType, s)) <<
          (holds += s)
      case PlanningStructureAdded(s: Change) =>
        existingSupportType.addInstance(SupportByExistingChange(s),
                                        1 + potentialSupports.actionPotentialSupports.size + changes.size) >>
          csp.post(s.persists.start <= s.persists.end) >>
          foreach(holds)(h => csp.post(Threat(s, h))) >>
          foreach(changes)(c => csp.post(Threat(c, s))) <<
          (changes += s)
      case PlanningStructureAdded(PlanningConstraint(c)) =>
        csp.post(c)

      case _ => consistent // not a causal structure, ignore
    }
  }

  def clone(newContext: PlanningHandler): CausalHandler = new CausalHandler(newContext, Some(this))
}
