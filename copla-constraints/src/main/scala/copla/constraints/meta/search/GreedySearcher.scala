package copla.constraints.meta.search

import copla.constraints.meta.{CSP, CSPView}
import copla.constraints.meta.decisions.DecisionOption
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

trait Searcher {
  def search(csp: CSP): SearchResult
}

object GreedySearcher extends slogging.StrictLogging {

  sealed trait Context {
    def csp: CSPView
    def depth: Int
  }
  private case class ContextImpl(csp: CSPView, depth: Int) extends Context

  def search(csp: CSP, picker: OptionPicker, stopCondition: Context => Boolean): SearchResult = {
    searchRec(csp.clone, picker, stopCondition, curDepth = 0)
  }

  @tailrec
  private def searchRec(csp: CSP, picker: OptionPicker, stopCondtion: Context => Boolean, curDepth: Int): SearchResult = {
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
      .sortBy(_.numOptions)

    // no decision left, success!
    if (decisions.isEmpty) {
      println(s"Got solution of makespan: " + implCSP.makespan)
      return Solution(implCSP)
    }

    val decision = decisions.head

    logger.debug(s"Decision (#opts: ${decision.numOptions}): $decision with options ${decision.options}")
    if(decision.numOptions == 0)
      return NoSolutionFound(Some(implCSP))
    val opt = picker.pick(decision.options)

    logger.debug(s"option: $opt")
    opt.enforceIn(implCSP)

    searchRec(implCSP, picker, stopCondtion, curDepth +1)
  }

}
