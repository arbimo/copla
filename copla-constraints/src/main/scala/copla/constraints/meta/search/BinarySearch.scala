package copla.constraints.meta.search

import copla.constraints.meta._
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.updates._

sealed trait SearchResult {
  def isSolution: Boolean
}

case class Solution(csp: CSP) extends SearchResult {
  assert3(csp.isSolution)
  override def isSolution = true
}

object Unsolvable extends SearchResult {
  override def isSolution: Boolean = false
}
object NoSolutionFound extends SearchResult {
  override def isSolution: Boolean = false
}

case class Crash(cause: FatalError) extends SearchResult {
  override def isSolution = false
}

object BinarySearch extends slogging.StrictLogging {
  var count = 0

  def search(_csp: CSP, optimizeMakespan: Boolean = false, curDepth: Int = 0): SearchResult = {
    logger.debug(s"Depth First Search: $curDepth")
    count += 1
    implicit val csp = _csp

    csp.propagate() match {
      case Consistent(_)  => // continue
      case x: Inconsistent => return Unsolvable
      case x: FatalError   => return Crash(x)
    }

    // variables by increasing domain size
    val decisions = csp.decisions.pending
      .filter(_.pending)
      .sortBy(_.numOptions)

    // no decision left, success!
    if (decisions.isEmpty) {
      println(s"Got solution of makespan: " + csp.makespan)
      return Solution(csp)
    }

    val decision = decisions.head

    val base: CSP                 = csp.clone
    var res: Option[SearchResult] = None

    logger.debug(s"Decision: $decision with options ${decision.options}")
    val options = Seq(decision.options.head, decision.options.head.negate)
    for (opt <- options) {
      logger.debug(s"option: $opt")
      val cloned = base.clone
      opt.enforceIn(cloned)
      search(cloned, optimizeMakespan, curDepth+1) match {
        case Solution(sol) if !optimizeMakespan =>
          return Solution(sol)
        case Solution(sol) =>
          // enforce better makespan for future branches
          base.post(base.temporalHorizon < sol.makespan)
          res = Some(Solution(sol))
        case Unsolvable =>
        case NoSolutionFound =>
          return Crash(fatal("BinarySearch is exhaustive and should return Solution | Unsolvable."))
        case x: Crash =>
          return x
      }
    }
    res.getOrElse(Unsolvable)
  }
}
