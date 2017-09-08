package copla.planning.types

import copla.anml.model.SymFunction
import copla.constraints.meta.types.statics.{BaseType, Type}

class FunctionVarType(val functions: Iterable[SymFunction])
  extends BaseType[SymFunction]("TFunction", functions.toList.zipWithIndex) {

}
