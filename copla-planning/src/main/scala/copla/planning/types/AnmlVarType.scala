package copla.planning.types

import copla.constraints.meta.types.statics.{BaseType, Type}

class AnmlVarType(name: String, val anmlType: Any /* TODO:SimpleType */)
  extends BaseType[String](name, Seq() /* TODO: anmlType.instances.toList.map(i => (i.instance, i.id))*/) {

}
