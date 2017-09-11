package copla.planning.types

import copla.constraints.meta.types.statics.{BaseType, Type}

class FunctionVarType(val functions: Iterable[Any /*TODO:SymFunction*/])
  extends BaseType[Any /*TODO:SymFunction*/]("TFunction", functions.toList.zipWithIndex) {

}
