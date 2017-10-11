package copla.planning.causality

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.constraints._

import scala.collection.mutable
import copla.constraints.meta.domains.{Domain, EmptyDomain}
import copla.constraints.meta.events.Event
import copla.constraints.meta.types.statics.Type
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.{IVar, IntVar, IntVariable, VarWithDomain}
import copla.lang.model.core
import copla.planning.structures.Holds
import copla.planning.variables.InstanceVar

class PotentialSupport(context: CausalHandler) {

  private val planner = context.context
  private val pb      = planner.pb

  val actionPotentialSupports: mutable.Map[core.ActionTemplate, ActionPotentialSupport] =
    mutable.Map()

  for (a <- pb.actionTemplates) {
    actionPotentialSupports.put(a, new ActionPotentialSupport(a, context))
  }

  def report: String =
    "--- Potential supports ---\n" +
      actionPotentialSupports.values.map(_.report).mkString("\n")

  def clone(newContext: CausalHandler): PotentialSupport = this // should be perfectly immutable
}

class ActionPotentialSupport(val act: core.ActionTemplate, private var context: CausalHandler) {

  /** Constraint require that the given variable take a value in the given domain. TODO */
  class InDomain(variable: IntVariable, domain: Domain) extends Constraint {
    override def variables(implicit csp: CSPView): Set[IVar] = Set(variable)
    override def satisfaction(implicit csp: CSPView): Satisfaction =
      if (variable.domain.emptyIntersection(domain)) ConstraintSatisfaction.VIOLATED
      else if (variable.domain.containedBy(domain)) ConstraintSatisfaction.SATISFIED
      else ConstraintSatisfaction.UNDEFINED

    override def propagate(event: Event)(implicit csp: CSPView): PropagationResult =
      if (variable.domain.emptyIntersection(domain))
        Inconsistency
      else if (variable.domain.containedBy(domain))
        Satisfied()
      else
        Satisfied(UpdateDomain(variable, variable.domain & domain))

    /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
    override def reverse: Constraint = ???
  }

  /** Constraint that requires the N given variables to take values in the N given domains. */
  class PotentialSupportFeasibility(vars: Seq[IntVariable], domains: Seq[Domain])
      extends ConjunctionConstraint(vars.zip(domains).map(x => new InDomain(x._1, x._2))) {
    require(vars.size == domains.size)
  }
//
  val byFunction: mutable.Map[core.FunctionTemplate, Array[Domain]] = mutable.Map()

  private val planner = context.context
  private val pb      = planner.pb

  for (s <- act.content.collect { case x: core.TimedAssertion with core.ProvidesChange => x }) {
    val funcDom  = planner.func(s.fluent.template).initialDomain(planner.csp)
    val argDoms  = s.fluent.params.map(arg => domainOf(arg))
    val valueDom = domainOf(s.valueAfterChange)

    val domains = funcDom +: argDoms :+ valueDom
    if (DEBUG_LEVEL >= 1) {
      for ((dom, typ) <- domains.zip(typesOf(s.fluent.template)))
        assert1(
          dom.values.forall(typ.hasValue),
          s"In action $act, the assertion $s has at least one variable that does not match the function's types.")
    }
    add(s.fluent.template, domains)
  }

  private def add(func: core.FunctionTemplate, doms: Seq[Domain]) {
    val arr = byFunction.getOrElseUpdate(func, Array.fill(doms.size)(new EmptyDomain))
    for (i <- arr.indices)
      arr(i) = arr(i) + doms(i)
  }

  private def domainOf(localVar: core.Var): Domain = {
    planner.variables.get(localVar) match {
      case Some(variable) => // variable already exists, use its initial domain
        variable.initialDomain(planner.csp)
      case None =>
        localVar match {
          case v: core.Instance =>
            new InstanceVar(v, planner.types.get(v.typ)).initialDomain(planner.csp)
          case _ =>
            // variable is not declared yet (e.g. can be one of the action arguments), use its type to infer possible values
            planner.types.get(localVar.typ).asDomain
        }
    }
  }

  private def typesOf(func: core.FunctionTemplate): Seq[Type[_]] = {
    planner.types.functionType +: (func.params.map(_.typ) :+ func.typ).map(planner.types.get)
  }

  /** Provides a constraint that is violated iff the current action cannot support the given Holds */
  def potentialSupportConstraint(target: Holds): Constraint = {
    byFunction.get(target.fluent.func.f) match {
      case Some(doms) =>
        val vars = target.fluent.func +: target.fluent.params :+ target.value
        new PotentialSupportFeasibility(vars, doms) {
          override def toString = s"support of [$target] by [${ActionPotentialSupport.this.act}]"
        }
      case None =>
        new Contradiction
    }
  }

  def report: String = {
    val sb = new StringBuilder
    sb.append(s"Supports for action $act\n")
    for (f <- byFunction.keys) {
      sb.append("  ")
      sb.append(
        byFunction(f)
          .zip(typesOf(f))
          .map { case (dom, typ) => typ.viewOf(dom).toString }
          .mkString("  "))
      sb.append("\n")
    }
    sb.toString()
  }
}
