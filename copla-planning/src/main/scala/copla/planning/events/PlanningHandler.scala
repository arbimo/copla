package copla.planning.events

import copla.constraints.meta.{CSP, CSPUpdateResult, Consistent, FatalError}
import copla.constraints.meta.constraints.ExtensionConstraint
import copla.constraints.meta.domains.ExtensionDomain
import copla.constraints.meta.events.{Event, InternalCSPEventHandler}
import copla.constraints.meta.stn.constraint.{Contingent, MinDelay}
import copla.constraints.meta.stn.variables.{RelativeTimepoint, Timepoint}
import copla.constraints.meta.util.Assertion._
import copla.planning.causality.CausalHandler
import copla.planning.causality.support.SupportByAction
import copla.planning.model.{Chronicle, Problem}
import copla.planning.structures.{CausalStruct, Change, Holds}
import copla.planning.types.{AnmlVarType, TypeHandler}
import copla.planning.variables.{FVar, InstanceVar, SVar, Var}
import copla.lang.model.core
import slogging.StrictLogging

import scala.collection.mutable

class PlanningHandler(_csp: CSP, base: Either[Problem, PlanningHandler])
    extends InternalCSPEventHandler
    with StrictLogging {

  implicit val csp = _csp

  val log = logger

  assert1(!csp.conf.enforceTpAfterStart,
          "Planner needs to be able some timepoints before the CSP's temporal origin.")

  val pb: Problem = base match {
    case Left(anmlProblem) => anmlProblem
    case Right(prev)       => prev.pb
  }

  val variables: mutable.Map[core.Var, Var] = base match {
    case Right(prev) => prev.variables.clone()
    case Left(_)     => mutable.Map()
  }

  val stateVariables: mutable.Map[core.Function, SVar] = base match {
    case Right(prev) => prev.stateVariables.clone()
    case Left(_)     => mutable.Map()
  }

  val functionVars: mutable.Map[core.FunctionTemplate, FVar] = base match {
    case Left(_)     => mutable.Map()
    case Right(prev) => prev.functionVars.clone()
  }

  val types: TypeHandler = base match {
    case Left(_)     => new TypeHandler(pb)
    case Right(prev) => prev.types
  }

  val actions: mutable.ArrayBuffer[core.Action] = base match {
    case Right(prev) => prev.actions.clone()
    case Left(_)     => mutable.ArrayBuffer()
  }

  val extensionDomains: mutable.Map[core.ConstantTemplate, ExtensionDomain] = base match {
    case Right(prev) => prev.extensionDomains.clone()
    case Left(_)     => mutable.Map()
  }

  // last since causal handler typically need to access types and variables
  val subhandlers: mutable.ArrayBuffer[PlanningEventHandler] = base match {
    case Left(_)     => mutable.ArrayBuffer(new CausalHandler(this))
    case Right(prev) => prev.subhandlers.map(_.clone(this))
  }

  def variable(v: core.Var): Var = v match {
    case v: core.Instance =>
      variables.getOrElseUpdate(v, new InstanceVar(v, types.get(v.typ)))
    case _ =>
      variables.getOrElseUpdate(v, new Var(v, types.get(v.typ)))
  }

  def func(f: core.FunctionTemplate): FVar =
    functionVars.getOrElseUpdate(f, new FVar(f, types.functionType))

  def sv(psv: core.Function): SVar =
    stateVariables.getOrElseUpdate(psv, new SVar(func(psv.template), psv.params.map(variable), psv))

  def getHandler[T](clazz: Class[T]): T = subhandlers.filter(_.getClass == clazz).toList match {
    case Nil      => throw new IllegalArgumentException("No handler of such type")
    case h :: Nil => h.asInstanceOf[T]
    case list     => throw new IllegalArgumentException("Multiple handlers of such type")
  }

  def tp(tpRef: core.TPRef): RelativeTimepoint = {
    val absoluteTimepoint =
      if (tpRef.id == pb.start.id)
        csp.temporalOrigin
      else if (tpRef.id == pb.end.id)
        csp.temporalHorizon
      else
        csp.varStore.getTimepoint(tpRef)
    RelativeTimepoint(absoluteTimepoint, tpRef.delay)
  }

  def insertChronicle(chronicle: Chronicle): CSPUpdateResult = {
    Consistent ==> CSPUpdateResult.thenForEach[core.Statement](
      chronicle.content, {
        case _: core.Declaration[_] =>
          Consistent
        case core.BindAssertion(constant, v) =>
          val variables = (constant.params :+ v).map(variable)
          csp.post(
            new ExtensionConstraint(variables,
                                    extensionDomains.getOrElseUpdate(constant.template,
                                                                     new ExtensionDomain(variables.size))))
        case core.StaticEqualAssertion(left, right) =>
          csp.post(variable(left) === variable(right))
        case core.StaticDifferentAssertion(left, right) =>
          csp.post(variable(left) =!= variable(right))

        case core.StaticAssignmentAssertion(left, right) =>
          val func = left.template
          assert1((left.params :+ right).forall(variable(_).domain.isSingleton))
          val values = (left.params :+ right).map(variable(_).domain.head)
          extensionDomains.getOrElseUpdate(func, new ExtensionDomain(values.size)).addTuple(values)
          Consistent

        case ass: core.TimedAssertion =>
          val structs = CausalStruct.assertionAsPlanningStructs(ass, this)
          structs.foldLeft(Consistent: CSPUpdateResult)((status, struct) =>
            status ==> csp.addEvent(PlanningStructureAdded(struct)))

        case core.TBefore(from, to) =>
          csp.post(tp(from) <= tp(to))
      }
    )
  }
  //    for(c <- chronicle.bindingConstraints.asScala)  c match {
  //      case c: VarInequalityConstraint =>
  //        csp.post(variable(c.leftVar) =!= variable(c.rightVar))
  //      case c: VarEqualityConstraint =>
  //        csp.post(variable(c.leftVar) === variable(c.rightVar))
  //      case c: AssignmentConstraint =>
  //        val func = c.sv.func
  //        assert1((c.sv.args.toList :+ c.variable).forall(variable(_).domain.isSingleton))
  //        val values = (c.sv.args.toList :+ c.variable).map(variable(_).domain.head)
  //        extensionDomains.getOrElseUpdate(func, new ExtensionDomain(values.size)).addTuple(values)
  //      case c: EqualityConstraint =>
  //        val variables = (c.sv.args.toList :+ c.variable).map(variable(_))
  //        csp.post(new ExtensionConstraint(variables, extensionDomains.getOrElseUpdate(c.sv.func, new ExtensionDomain(variables.size))))
  //      case x =>
  //        throw new NotImplementedError(s"Support for constraint $x is not implemented.")
  //    }
  //    for(c <- chronicle.temporalConstraints.asScala) c match {
  //      case c: MinDelayConstraint =>
  //        assert1(c.minDelay.isKnown, "Support for mixed constraint is not implemented yet.")
  //        csp.post(new MinDelay(tp(c.src), tp(c.dst), c.minDelay.get))
  //      case c: ContingentConstraint =>
  //        assert1(c.min.isKnown && c.max.isKnown, "Support for mixed constraints is not implemented yet")
  //        csp.post(new Contingent(tp(c.src), tp(c.dst), c.min.get, c.max.get))
  //    }
  //    for(s <- chronicle.logStatements.asScala) s match {
  //      case s: Persistence =>
  //        csp.post(tp(s.start) <= tp(s.end)) // todo: double check if those are needed
  //        csp.addEvent(PlanningStructureAdded(Holds(s, this)))
  //      case s: Transition =>
  //        csp.post(tp(s.start) < tp(s.end))
  //        csp.addEvent(PlanningStructureAdded(Change(s, this)))
  //        csp.addEvent(PlanningStructureAdded(Holds(s, this)))
  //      case s: Assignment =>
  //        csp.post(tp(s.start) < tp(s.end))
  //        csp.addEvent(PlanningStructureAdded(Change(s, this)))
  //    }
  //  }

  override def handleEvent(event: Event): CSPUpdateResult = {
    val res: CSPUpdateResult = event match {
      case InitPlanner =>
        csp.addEvent(ChronicleAdded(pb.chronicle))
      case ChronicleAdded(chronicle) =>
        insertChronicle(chronicle)
      case ActionInsertion(actionTemplate, support) =>
        val instance = actionTemplate.instance(actionTemplate.name+"_"+actions.size)
        actions += instance
        insertChronicle(new Chronicle(instance.content)) =!> {
          csp.post(csp.temporalOrigin <= tp(instance.start))
          support match {
            case Some(supportVar) => csp.post(new SupportByAction(instance, supportVar))
            case None             =>
          }
        }
      case _: PlanningStructureAdded => Consistent
      case event: PlanningEvent =>
        throw new NotImplementedError(s"The event $event is not handle")
      case _ => Consistent // not concerned by this event
    }
    res ==> CSPUpdateResult.thenForEach[PlanningEventHandler](subhandlers, _.handleEvent(event))
  }

  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new PlanningHandler(newCSP, Right(this))

  def report: String = {
    val sb = new StringBuilder
    for (h <- subhandlers) {
      sb.append(s"\n--------- SubHandler report: $h -------\n")
      sb.append(h.report)
    }
    sb.append("---------- Actions ---------\n")
    for (a <- actions.sortBy(a => tp(a.start).domain.lb)) {
      val argsString = a.args.map(a => variable(a).dom).mkString(", ")
      sb.append(s"[${tp(a.start).domain.lb}, ${tp(a.end).domain.lb}] ${a.name}($argsString)\n")
    }
    sb.toString()
  }
}
