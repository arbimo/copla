package copla.constraints.meta.types.dynamics

import copla.constraints.meta.CSP
import copla.constraints.meta.types.statics.{ComposedType, Type}
import copla.constraints.meta.types.statics.Type

class ComposedDynamicType[T](val directSubTypes: Seq[DynamicType[T]]) extends DynamicType[T] {
  override val isStatic: Boolean = directSubTypes.forall(_.isStatic)

  override def subTypes: Seq[DynamicType[T]] = directSubTypes ++ directSubTypes.flatMap(_.subTypes)

  override lazy val defaultStatic: Type[T] =
    new ComposedType[T](directSubTypes.map(_.defaultStatic))

  override def static(implicit csp: CSP): Type[T] =
    new ComposedType[T](directSubTypes.map(_.static))
}
