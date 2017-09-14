package copla.planning.types

import copla.lang.model.core
import copla.planning.model.Problem

class TypeHandler(pb: Problem) {

  val types: Map[core.Type, AnmlVarType] = {
    val instances: Seq[core.Instance] = pb.anml.collect {
      case core.InstanceDeclaration(instance) => instance
    }
    val instancesWithId: Seq[(core.Instance, Int)] = instances.zipWithIndex
    val types                                      = pb.anml.collect { case core.TypeDeclaration(typ) => typ }
    types
      .map(t => {
        val instancesOfThisType: Seq[(core.Instance, Int)] =
          instancesWithId.filter(instanceAndId => instanceAndId._1.typ.isSubtypeOf(t))
        (t, new AnmlVarType(t.id.toString, t, instancesOfThisType))
      })
      .toMap
  }

  val functionType: FunctionVarType = new FunctionVarType(pb.anml.collect {
    case core.FunctionDeclaration(funcTemplate) => funcTemplate
  })

  def get(t: core.Type): AnmlVarType = types(t)

}
