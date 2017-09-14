package copla.planning.types

import copla.constraints.meta.types.statics.{BaseType, Type}
import copla.lang.model.{core => anml}

class FunctionVarType(val functions: Iterable[anml.FunctionTemplate])
  extends BaseType[anml.FunctionTemplate]("TFunction", functions.toList.zipWithIndex) {

}
