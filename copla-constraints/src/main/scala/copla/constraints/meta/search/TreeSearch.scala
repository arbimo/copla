package copla.constraints.meta.search

import copla.constraints.meta._
import copla.constraints.meta.decisions.DecisionOption
import copla.constraints.bindings.InconsistentBindingConstraintNetwork
import copla.constraints.meta.constraints.Inconsistency

import scala.collection.mutable
import copla.constraints.meta.updates._
import cats.implicits._

class TreeSearch(nodes: Seq[CSP]) extends slogging.StrictLogging {

  private var numExpansions       = 0
  private var numAppliedDecisions = 0

  private var queue = mutable.PriorityQueue[CSP]()
  nodes.foreach(n => queue.enqueue(n))

  def incrementalDeepeningSearch(maxDepth: Int = Integer.MAX_VALUE): SearchResult = {
    logger.debug("Starting incremental deepening search.")
    val startTimeMs = System.currentTimeMillis()
    val backupQueue = queue.clone()
    for (i <- 0 to maxDepth) {
      logger.debug(s"DFS up to depth $i")
      queue = backupQueue.clone()
      search(i) match {
        case Solution(solution) =>
          logger.info(
            s"Solution found in ${System.currentTimeMillis() - startTimeMs}ms with $numExpansions/$numAppliedDecisions expansions/decisions up to depth $i.")
          return Solution(solution)
        case NoSolution =>
          logger.info("Problem has no solutions")
          return NoSolution
        case NoSolutionBelowDepth(_) =>
          logger.debug("No solution for this depth.")
        // continue
        case x: Crash => return x
      }
      if (i == maxDepth)
        return NoSolutionBelowDepth(maxDepth)
    }

    logger.info(
      s"No solution found after $numExpansions expansions (in ${System.currentTimeMillis() - startTimeMs}ms)")
    NoSolution
  }

  private def applyTrivialDecisions(_csp: CSP, maxDecisionsToApply: Int): Update = {
    implicit val csp = _csp
    csp.propagate() >> {
      if (maxDecisionsToApply == 0)
        consistent
      else {
        csp.decisions.pending.find(dec => dec.pending && dec.numOption <= 1) match {
          case None => consistent
          case Some(dec) if dec.numOption == 0 =>
            inconsistent("Flaw with no resolver: " + dec)
          case Some(dec) if dec.numOption == 1 =>
            dec.options.head.enforceIn(csp)
            applyTrivialDecisions(csp, maxDecisionsToApply - 1)
          case _ => fatal("should be unreachable")
        }
      }
    }
  }

  def search(maxDepth: Int = Integer.MAX_VALUE): SearchResult = {
    var maxDepthReached = false
    while (queue.nonEmpty) {

      implicit val csp = queue.dequeue()
      //      println(" "*cur.depth + "X" + " "*(maxDepth-cur.depth-1)+"|")
      numExpansions += 1

      applyTrivialDecisions(csp, 50) >>
        csp.propagate() << {
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

        def apply(csp: CSP, decision: DecisionOption): Option[CSP] = {
          decision.enforceIn(csp)
          csp.propagate() match {
            case Consistent(_) => Some(csp)
            case _         => None // TODO handle FatalError properly
          }
        }

        val children = decision.options.flatMap(opt => apply(csp.clone, opt))
        numAppliedDecisions += children.size
        for (x <- children)
          if (x.depth <= maxDepth)
            queue.enqueue(x)
          else
            maxDepthReached = true
      } match {
        case er: FatalError => return Crash(er)
        case _ =>
      }
    }

    if (maxDepthReached)
      NoSolutionBelowDepth(maxDepth)
    else
      NoSolution
  }

}
