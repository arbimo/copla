package copla.planning.variables

import copla.anml.model
import copla.anml.model.{ParameterizedStateVariable, SymFunction}
import copla.anml.model.concrete.{InstanceRef, VarRef}
import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.{Constraint, Contradiction, Tautology}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.types.statics.{TypedVariable, TypedVariableWithInitialDomain}
import copla.constraints.meta.variables.VariableSeq
import copla.planning.types.{AnmlVarType, FunctionVarType}

/** Any ANML variable (e.g. action parameters or declared with constant) */
class Var(v: VarRef, typ: AnmlVarType) extends TypedVariable[String](typ, Some(v))

/** Variable that represents a particular instance. Its domain is a singleton containing this particular instance. */
class InstanceVar(v: InstanceRef, typ: AnmlVarType) extends Var(v, typ) {
  override def initialDomain(implicit csp: CSP) : Domain = Domain(Set(typ.instanceToInt(v.instance)))
  override val unaryConstraints = List(this === v.instance)
}

/** A variable representing a particular ANML function (e.g. the "position" of "position(x)".
  * Its domain is always a singleton, containing the SymFunction itself. */
class FVar(val f: model.SymFunction, typ: FunctionVarType)
  extends TypedVariable[SymFunction](typ, Some(f))
{
  override def initialDomain(implicit csp: CSP) : Domain = Domain(Set(typ.instanceToInt(f)))
  override val unaryConstraints = List(this === f)
}

/** A state variable, defined as a sequence of an ANML function (FVar) and parameters of the state variable */
class SVar(val func: FVar, val params: Seq[Var], ref: ParameterizedStateVariable)
  extends VariableSeq(func :: params.toList, Some(ref)) {
  require(func.f.argTypes.size == params.size)

  def =!=(svar: SVar) : Constraint =
    if(func == svar.func)
      super.=!=(svar)
    else
      new Tautology

  def ===(svar: SVar) : Constraint =
    if(func == svar.func)
      super.===(svar)
    else
      new Contradiction
}