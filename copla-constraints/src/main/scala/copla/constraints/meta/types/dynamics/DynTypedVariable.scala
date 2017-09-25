package copla.constraints.meta.types.dynamics

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.types.statics.{DomainView, Type}
import copla.constraints.meta.types.statics.Type
import copla.constraints.meta.variables.IntVariable

/** Variable with a dynamic type.
  * If an instance is added to the dynamic type, the domain of this variable is extended with the corresponding value. */
class DynTypedVariable[T](val typ: DynamicType[T], ref: Option[Any] = None)
    extends IntVariable(ref) {

  def this(ref: Any, typ: Type[T]) = this(typ, Some(ref))

  override def initialDomain(implicit csp: CSPView): Domain = typ.static.asDomain

  def dom(implicit csp: CSPView): DomainView[T] = new DomainView[T](domain, typ.static)
}
