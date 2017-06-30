package copla.constraints.meta.variables

import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.domains.Domain

class ReificationVariable(initialDomain: Domain, val constraint: Constraint)
    extends BooleanVariable(initialDomain, Some(constraint)) {

  override def toString = s"rei($constraint)"
}
