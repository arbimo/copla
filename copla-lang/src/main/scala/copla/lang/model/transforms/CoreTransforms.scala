package copla.lang.model.transforms

import copla.lang.model.core._

object CoreTransforms {

  sealed trait ConstantAsLocalVar

  def replaceConstantsWithLocalVars[T <: CoreModel](modelT: T): T with ConstantAsLocalVar = {
    import landscaper._

    // fix type to help landscaper
    val model: CoreModel = modelT

    // extract all bind assertions on constant templates with no args
    val bindAssertionPattern = pattern {
      case BindAssertion(cst @ Constant(_, Seq()), variable) => Seq((variable, cst))
    }
    val bindAssertions: Seq[(LocalVar, Constant)] = extract(bindAssertionPattern, model)

    /** Transforms a constant template to a local var */
    def constantAsLocalVar(cst: ConstantTemplate): LocalVar = cst match {
      case ConstantTemplate(id, typ, Seq()) => LocalVar(id, typ)
      case _                                => sys.error("Cannot transform a parameterized constant to a local var: " + cst)
    }

    val swaps: Map[LocalVar, LocalVar] =
      bindAssertions.toMap.mapValues(cst => constantAsLocalVar(cst.template))
    val varSwapper: LocalVar => LocalVar = (x: LocalVar) => swaps.getOrElse(x, x)

    // erase variable that will be replaced, and the assertions that bind them
    val m1 = erase({
      case LocalVarDeclaration(v) => swaps.contains(v)
      case BindAssertion(cst, _)  => cst.template.params.isEmpty
    }: PartialFunction[Any, Boolean], model)

    // swap all vars to the new ones
    val m2 = rewrite(varSwapper, m1)

    // replace function declaration by local var declaration
    val declSwapper = (x: InModuleBlock) =>
      x match {
        case FunctionDeclaration(cst: ConstantTemplate) if cst.params.isEmpty =>
          LocalVarDeclaration(constantAsLocalVar(cst))
        case _ => x
      }
    val m3 = m2.map(declSwapper)

    m3.asInstanceOf[T with ConstantAsLocalVar]
  }

}
