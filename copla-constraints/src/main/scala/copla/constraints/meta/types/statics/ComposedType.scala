package copla.constraints.meta.types.statics

import copla.constraints.meta.util.Assertion._

/** A high level type as a union of several subtypes. */
class ComposedType[+T](val directSubTypes: Seq[Type[T]], val selfInstances: Seq[(T, Int)] = Seq())
    extends Type[T] {

  override final def subTypes: Seq[Type[T]] = directSubTypes ++ directSubTypes.flatMap(_.subTypes)

  def instances: Seq[T] = {
    val all = directSubTypes.flatMap(st => st.instances).toList ++ selfInstances.map(_._1)
    assert1(all.distinct.size == all.size, s"Some instances are duplicated in type $this")
    assert1(all.map(instanceToInt).distinct.size == all.size,
            s"Some instances have the same value in type $this")
    all
  }

  override def instanceToIntOption[ST >: T](instance: ST): Option[Int] = {
    selfInstances
      .find(_._1 == instance)
      .map(_._2)
      .orElse(
        directSubTypes
          .find(t => t.hasInstance(instance))
          .flatMap(t => t.instanceToIntOption(instance)))
  }

  override def intToInstanceOption(value: Int): Option[T] = {
    selfInstances
      .find(_._2 == value)
      .map(_._1)
      .orElse(
        directSubTypes.find(t => t.hasValue(value)).flatMap(t => t.intToInstanceOption(value)))
  }

  /** Returns true if this type has instance with the given int representation. */
  override def hasValue(value: Int): Boolean = directSubTypes.exists(_.hasValue(value))

  override def withoutInstance[ST >: T](instance: ST): Type[T] =
    throw new RuntimeException("Cannot remove an instance from a composed type.")

  override def withInstance[ST >: T](instance: ST, value: Int): Type[T] =
    throw new RuntimeException("Cannot add an instance to a composed type.")

  override def defaultStatic: Type[T] = this
}
