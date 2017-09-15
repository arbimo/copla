package copla.constraints.meta

import copla.constraints.meta.constraints.ConstraintSatisfaction.UNDEFINED
import org.scalatest.{BeforeAndAfter, FunSuite}

class StnTest extends FunSuite with BeforeAndAfter {

  implicit var csp: CSP = _

  before {
    csp = new CSP
  }


  test("Simple STN with reification") {

    val tp1 = csp.varStore.getTimepoint("first")
    val tp2 = csp.varStore.getTimepoint("second")
    val tp3 = csp.varStore.getTimepoint("third")

    val rei = csp.reified(tp3 < 2)
    val rei2 = csp.reified(tp1 < tp3)
    csp.propagate()
    csp.post(tp1 < tp2)
    csp.post(tp2 < tp3)
    csp.post(csp.temporalHorizon <= 100)

    csp.propagate()

    assert(rei.constraint.isViolated)
    assert(rei2.constraint.isSatisfied)
    assert(rei.isFalse)
    assert(rei2.isTrue)
  }

  test("Timepoint manipulation") {

    val tp1 = csp.varStore.getTimepoint("first")
    val tp2 = csp.varStore.getTimepoint("second")
    val tp3 = csp.varStore.getTimepoint("third")

    val delay = (tp1 +10) - tp2
    val c = delay <= 10
    assert(c.src == tp1 && c.dst == tp2 && c.minDelay == 0)

    csp.post(tp1 === 10)
    csp.post(tp2 === 20)
    csp.post(tp3 > tp1 && tp3 < tp2)
    csp.propagate()

    assert((tp1 < tp2).isSatisfied)
    assert((tp1 +10 <= tp2).isSatisfied)
    assert((tp1 +10 < tp2).isViolated)
    assert(tp3.domain.lb == 11)
    assert(tp3.domain.ub == 19)

    assert((tp3 +10).domain.lb == 21)
    assert((tp3 +10).domain.ub == 29)
    assert((tp3 -10).domain.lb == 1)
    assert((tp3 -10).domain.ub == 9)

    assert((tp3 + 10 < tp2).isViolated)
    assert((tp3 < tp2 - 10).isViolated)
    assert((tp3 + 5 < tp2 - 5).isViolated)

    assert(((tp3 - tp1) < 10).isSatisfied)
    assert(((tp3 - tp1) < 9).satisfaction == UNDEFINED)
  }

  test("Very Simple STN with reification") {

//    val tp1 = csp.varStore.getTimepoint("first")

    val rei = csp.reified(csp.temporalHorizon < 2)
    csp.propagate()
    csp.post(csp.temporalHorizon < 2)
    csp.propagate()

    assert(rei.constraint.isSatisfied)
    assert(rei.isTrue)
  }
}
