package copla.constraints.meta.types.statics

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.domains.Domain

class TypedVariableWithInitialDomain[T](typ: Type[T], values: Set[T], ref: Option[Any] = None)
    extends TypedVariable[T](typ, ref) {

  override def initialDomain(implicit csp: CSPView): Domain = Domain(values.map(typ.instanceToInt))
}
