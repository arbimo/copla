package copla.constraints.meta.search

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.decisions.{Decision, DecisionOption}
import copla.constraints.meta.updates._

import scala.annotation.tailrec

trait OptionPicker {
  def pick(options: Seq[DecisionOption]): DecisionOption
}

object OptionPicker {

  def randomized(seed: Int): OptionPicker = new OptionPicker {
    val r = new scala.util.Random(seed)
    r.nextInt(2) // for some reason, the first number is always the same
    override def pick(options: Seq[DecisionOption]): DecisionOption = {
      require(options.nonEmpty)
      val i = r.nextInt(options.size)
      options(i)
    }
  }
}

trait DecisionOrdering {

  /** Lower means higher priority */
  def priority(decision: Decision)(implicit csp: CSPView): Float

}

trait Searcher {
  def search(csp: CSP): SearchResult
}

object GreedySearcher extends slogging.StrictLogging {

  sealed trait Context {
    def csp: CSPView
    def depth: Int
  }
  private case class ContextImpl(csp: CSPView, depth: Int) extends Context

  def search(csp: CSP, decOrd: CSP => DecisionOrdering, picker: OptionPicker, stopCondition: Context => Boolean): SearchResult = {
    searchRec(csp.clone, decOrd(csp), picker, stopCondition, curDepth = 0)
  }

  @tailrec
  private def searchRec(csp: CSP, decOrder: DecisionOrdering, picker: OptionPicker, stopCondtion: Context => Boolean, curDepth: Int): SearchResult = {
    if(stopCondtion(ContextImpl(csp, curDepth))) {
      logger.debug("Stop condition triggered.")
      return NoSolutionFound(Some(csp))
    }

    logger.debug(s"Greedy Search: depth=$curDepth")

    // simply to make the implicit available
    implicit val implCSP = csp

    implCSP.propagate() match {
      case Consistent(_)  => // continue
      case x: Inconsistent =>
        logger.debug("Inconsistency")
        return NoSolutionFound(Some(implCSP))
      case x: FatalError   => return Crash(x)
    }

    // variables by increasing domain size
    val decisions = implCSP.decisions.pending
      .filter(_.pending)
      .sortBy(decOrder.priority)

    // no decision left, success!
    if (decisions.isEmpty) {
      logger.info(s"Got solution of makespan: " + implCSP.makespan)
      return Solution(implCSP)
    }

    val decision = decisions.head

    logger.debug(s"Decision (#opts: ${decision.numOptions}): $decision with options ${decision.options}")
    if(decision.numOptions == 0)
      return NoSolutionFound(Some(implCSP))
    val opt = picker.pick(decision.options)

    logger.debug(s"option: $opt")
    opt.enforceIn(implCSP)

    searchRec(implCSP, decOrder, picker, stopCondtion, curDepth +1)
  }

}
