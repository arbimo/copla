package copla.constraints.meta.types.statics

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.types.dynamics.DynamicType

/** A static type composed of a set of instances and a mapping from
  * domain values to instances (and vice versa).
  *
  * @tparam T Type of the instances
  */
trait Type[+T] extends DynamicType[T] {

  override def isStatic = true

  override def static(implicit csp: CSPView) = this

  override def subTypes: Seq[Type[T]]

  /** All instances of this type */
  def instances: Seq[T]

  /** Retrieves the int representation of given instance of this type.
    * Each instance must have a distinct int value. */
  def instanceToIntOption[ST >: T](instance: ST): Option[Int]

  /** Retrieves the instance associated with tis type. */
  def intToInstanceOption(value: Int): Option[T]

  def instanceToInt[ST >: T](instance: ST): Int =
    instanceToIntOption(instance) match {
      case Some(x) => x
      case None    => sys.error(s"Type $this has no instance $instance")
    }

  def intToInstance(value: Int): T = {
    intToInstanceOption(value) match {
      case Some(x) => x
      case None    => sys.error(s"Type $this has no instance represented by $value")
    }
  }

  /** Returns true if the given instance is part of this type. */
  def hasInstance[ST >: T](instance: ST) = instances.contains(instance)

  /** Returns true if this type has instance with the given int representation. */
  def hasValue(value: Int): Boolean

  def asDomain: Domain = Domain(instances.map(instanceToInt(_)).toSet)

  def viewOf(dom: Domain): DomainView[T] = new DomainView[T](dom, this)

  /** Returns a new version of this type with an additional instance. */
  def withInstance[ST >: T](instance: ST, value: Int): Type[T]

  /** Returns a new version of this type without the given instance. */
  def withoutInstance[ST >: T](instance: ST): Type[T]
}
