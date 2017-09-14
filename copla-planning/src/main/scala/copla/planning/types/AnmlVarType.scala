package copla.planning.types

import copla.constraints.meta.types.statics.{BaseType, Type}
import copla.lang.model.core

class AnmlVarType(name: String,
                  val anmlType: core.Type,
                  instancesWithId: Seq[(core.Instance, Int)])
    extends BaseType[core.Instance](name, instancesWithId)
