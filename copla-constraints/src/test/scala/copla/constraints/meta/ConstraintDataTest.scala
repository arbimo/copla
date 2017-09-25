package copla.constraints.meta

import copla.constraints.meta.constraints._
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events.Event
import copla.constraints.meta.variables.{IVar, IntVariable}
import org.scalatest.{BeforeAndAfter, FunSuite}


class ConstraintDataTest extends FunSuite with BeforeAndAfter{

  implicit var csp: CSP = null
  var v1: IntVariable = null

  class Counter(var i: Int) extends ConstraintData {
    override def clone(implicit newCSP: CSP): ConstraintData = new Counter(i)
  }

  class DomainSizeCounterConstraint(v: IntVariable) extends Constraint with WithData[Counter] {
    override def variables(implicit csp: CSPView): Set[IVar] = Set(v)

    override def onPost(implicit csp: CSPView): Seq[OnPostChange] = {
      DataInit(this, new Counter(v.domain.size)) :: Nil
    }

    override def propagate(event: Event)(implicit csp: CSPView): PropagationResult = {
      val update = DataUpdate(this, (c: Counter) => c.i = v.domain.size)
      Undefined(update)
    }

    override def satisfaction(implicit csp: CSPView): Satisfaction = ConstraintSatisfaction.UNDEFINED

    /** Returns the invert of this constraint (e.g. === for an =!= constraint) */
    override def reverse: Constraint = ???
  }

  before {
    csp = new CSP
    v1 = csp.variable("v1", Set(1,2,3))
  }

  test("data init") {
    val c = new DomainSizeCounterConstraint(v1)
    csp.post(c)
    csp.propagate()
    assert(c.data.i == 3)
    csp.updateDomain(v1, Domain(1,2,3,4,5))
    csp.propagate()
    assert(c.data.i == 5)
    csp.updateDomain(v1, Domain(1))
    csp.propagate()
    assert(c.data.i == 1)
  }


}
