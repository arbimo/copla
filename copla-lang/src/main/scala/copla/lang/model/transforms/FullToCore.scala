package copla.lang.model.transforms

import copla.lang.model._
import copla.lang.model.full.Scope

object FullToCore {

  private def staticExprToVar(expr: full.StaticSymExpr,
                              scope: Scope): (core.Var, Seq[core.Statement]) = {
    expr match {
      case x: core.Var => (x, Seq())
      case x: full.Constant[full.StaticSymExpr] =>
        val (params, statements) = x.params
          .map(param => staticExprToVar(param, scope))
          .foldLeft((Seq[core.Var](), Seq[core.Statement]())) {
            case ((params, statements), (param, additionalStmts)) =>
              (params :+ param, statements ++ additionalStmts)
          }
        val cst         = core.Constant(x.template, params)
        val variable    = core.LocalVar(scope.makeNewId(), cst.typ)
        val declaration = core.LocalVarDeclaration(variable)
        val eq          = core.BindAssertion(cst, variable)
        (variable, statements :+ eq :+ declaration)
    }
  }

  def trans(block: full.InModuleBlock): Seq[core.InModuleBlock] = block match {
    case x: core.InModuleBlock                => Seq(x)
    case x: full.StaticAssignmentAssertion[_] => Seq()

  }
}
