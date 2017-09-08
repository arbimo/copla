package copla.planning.types

import copla.anml.model.SimpleType
import copla.constraints.meta.types.statics.{BaseType, Type}

class AnmlVarType(name: String, val anmlType: SimpleType)
  extends BaseType[String](name, anmlType.instances.toList.map(i => (i.instance, i.id))) {

}
