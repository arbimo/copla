package copla.constraints.meta.search

import copla.constraints.meta._
import copla.constraints.meta.util.Assertion._

sealed trait SearchResult {
  def isSolution: Boolean
}

case class Solution(csp: CSP) extends SearchResult {
  assert3(csp.isSolution)
  override def isSolution = true
}

case class Failure(cause: Option[Inconsistent]) extends SearchResult {
  override def isSolution = false
}

case class Crash(cause: FatalError) extends SearchResult {
  override def isSolution = false
}

object BinarySearch {
  var count = 0

  def search(_csp: CSP, optimizeMakespan: Boolean = false): SearchResult = {
    count += 1
    implicit val csp = _csp

    csp.propagate() match {
      case Consistent      => // continue
      case x: Inconsistent => return Failure(Some(x))
      case x: FatalError   => return Crash(x)
    }

    // variables by increasing domain size
    val decisions = csp.decisions.pending
      .filter(_.pending)
      .sortBy(_.numOption)

    // no decision left, success!
    if (decisions.isEmpty) {
      println(s"Got solution of makespan: " + csp.makespan)
      return Solution(csp)
    }

    val decision = decisions.head

    val base: CSP                 = csp.clone
    var res: Option[SearchResult] = None

    for (opt <- decision.options) {
      val cloned = base.clone
      opt.enforceIn(cloned)
      search(cloned, optimizeMakespan) match {
        case Solution(sol) if !optimizeMakespan =>
          return Solution(sol)
        case Solution(sol) =>
          // enforce better makespan for future branches
          base.post(base.temporalHorizon < sol.makespan)
          res = Some(Solution(sol))
        case x: Failure =>
          res = res.orElse(Some(x))
        case x: Crash =>
          return x
      }
    }
    res.getOrElse(Failure(None))
  }
}
