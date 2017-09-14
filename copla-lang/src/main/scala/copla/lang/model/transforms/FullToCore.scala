package copla.lang.model.transforms

import copla.lang.model
import copla.lang.model._
import copla.lang.model.full.Scope

object FullToCore {

  private def staticExprToVar(expr: full.StaticSymExpr)(
      implicit scope: Scope): (core.Var, Seq[core.Statement]) = {
    expr match {
      case x: core.Var => (x, Seq())
      case x: full.Constant =>
        val (params, statements) = staticExprsToVars(x.params)
        val cst                  = core.Constant(x.template, params)
        val variable             = core.LocalVar(scope.makeNewId(), cst.typ)
        val declaration          = core.LocalVarDeclaration(variable)
        val eq                   = core.BindAssertion(cst, variable)
        (variable, statements :+ eq :+ declaration)
    }
  }

  private def staticExprsToVars(exprs: Seq[full.StaticSymExpr])(
      implicit scope: Scope): (Seq[core.Var], Seq[core.Statement]) =
    exprs
      .map(param => staticExprToVar(param))
      .foldLeft((Seq[core.Var](), Seq[core.Statement]())) {
        case ((params, statements), (param, additionalStmts)) =>
          (params :+ param, statements ++ additionalStmts)
      }

  def timedSymExpr2CoreFluent(expr: full.TimedSymExpr)(
      implicit scope: Scope): (core.Fluent, Seq[core.Statement]) = {
    expr match {
      case fluent: full.Fluent =>
        val (params, statements) = staticExprsToVars(fluent.params)
        (core.Fluent(fluent.template, params), statements)
    }
  }

  def trans(block: full.Statement)(implicit scope: Scope): Seq[core.Statement] = block match {
    case x: core.Statement => Seq(x)

    case x: full.StaticAssignmentAssertion =>
      (x.left, x.right) match {
        case (cst: full.Constant, inst: core.Instance)
            if cst.params.forall(_.isInstanceOf[core.Instance]) =>
          val boundCst = new model.core.BoundConstant(cst.template,
                                                      cst.params.map(_.asInstanceOf[core.Instance]))
          Seq(core.StaticAssignmentAssertion(boundCst, inst))
        case _ =>
          throw new UnsupportedOperationException(
            s"Assignment assertions on constant functions are only supported when all parameters are declared instances in: $block")
      }

    case x: full.StaticEqualAssertion =>
      val (lVar, lStmts) = staticExprToVar(x.left)
      val (rVar, rStmts) = staticExprToVar(x.right)
      lStmts ++ rStmts :+ core.StaticEqualAssertion(lVar, rVar)

    case x: full.StaticDifferentAssertion =>
      val (lVar, lStmts) = staticExprToVar(x.left)
      val (rVar, rStmts) = staticExprToVar(x.right)
      lStmts ++ rStmts :+ core.StaticEqualAssertion(lVar, rVar)

    case full.TemporallyQualifiedAssertion(interval, assertion) =>
      val baseStatements: Seq[core.Statement] = Seq(Seq(core.TimepointDeclaration(assertion.start)),
                               Seq(core.TimepointDeclaration(assertion.end)),
                               interval.start === assertion.start,
                               interval.end === assertion.end).flatten
      assertion match {
        case full.TimedEqualAssertion(fluent, value, parent, name) =>
          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
          val (coreValue, valueStatements)  = staticExprToVar(value)
          baseStatements ++ fluentStatement ++ valueStatements :+ core
            .TimedEqualAssertion(assertion.start, assertion.end, coreFluent, coreValue)
        case full.TimedAssignmentAssertion(fluent, value, parent, name) =>
          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
          val (coreValue, valueStatements)  = staticExprToVar(value)
          baseStatements ++ fluentStatement ++ valueStatements :+ core
            .TimedAssignmentAssertion(assertion.start, assertion.end, coreFluent, coreValue)
        case full.TimedTransitionAssertion(fluent, fromValue, toValue, parent, name) =>
          val (coreFluent, fluentStatement) = timedSymExpr2CoreFluent(fluent)
          val (coreFromValue, fromValueStatements)  = staticExprToVar(fromValue)
          val (coreToValue, toValueStatements)  = staticExprToVar(toValue)
          baseStatements ++ fluentStatement ++ fromValueStatements ++ toValueStatements :+ core
            .TimedTransitionAssertion(assertion.start, assertion.end, coreFluent, coreFromValue, coreToValue)
      }
  }

  def trans(model: full.Model): Seq[core.InModuleBlock] = {
    implicit val scope = model.scope
    model.store.blocks.flatMap {
      case x: core.InModuleBlock => Seq(x)
      case x: full.Statement => trans(x)
      case x: full.ActionTemplate => ???
    }

  }
}
