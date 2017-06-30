package copla.constraints.meta.types.dynamics

import copla.constraints.meta.types.statics.{BaseType, Type}
import copla.constraints.meta.types.statics.Type

class BaseDynamicType[T](val name: String, initialValues: Seq[(T, Int)]) extends DynamicType[T] {

  override def isStatic: Boolean = false

  override def subTypes: Seq[DynamicType[T]] = Nil

  override lazy val defaultStatic: Type[T] = new BaseType[T](name, initialValues)
}
