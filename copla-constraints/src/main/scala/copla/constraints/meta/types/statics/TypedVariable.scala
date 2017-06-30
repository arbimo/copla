package copla.constraints.meta.types.statics

import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.types.dynamics.DynTypedVariable

/** A variable with a static type and some convenience functions. */
class TypedVariable[T](override val typ: Type[T], ref: Option[Any] = None)
    extends DynTypedVariable[T](typ, ref) {

  def this(ref: Any, typ: Type[T]) = this(typ, Some(ref))

  def ===(instance: T): Constraint = this === typ.instanceToInt(instance)

  def =!=(instance: T): Constraint = this =!= typ.instanceToInt(instance)
}
