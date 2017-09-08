package copla.planning.types

import copla.anml.model
import copla.anml.model.{AnmlProblem, SimpleType, SymFunction}

class TypeHandler(pb: AnmlProblem) {

  val types: Map[SimpleType, AnmlVarType] = pb.instances.allSimpleTypes.map(t => (t, new AnmlVarType(t.name, t))).toMap

  val functionType: FunctionVarType = new FunctionVarType(pb.functions.all.collect{ case f: SymFunction => f })

  def get(t: model.Type) : AnmlVarType = t match {
    case t: SimpleType => types(t)
    case _ => throw new NotImplementedError()
  }

}
