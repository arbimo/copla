package copla.planning.search

import copla.constraints.meta.CSPView
import copla.constraints.meta.decisions.Decision
import copla.constraints.meta.search.DecisionOrdering
import copla.lang.analysis.DetailedKnoblock
import copla.planning.search.DecisionFeatures.Extractor

object DecisionOrderings {

  case class SeqFeatures(extractors: Seq[Extractor]) extends DecisionOrdering {
    override def priority(decision: Decision)(implicit csp: CSPView): Float = {
      var p = 0f
      for (ext <- extractors) {
        val feature = ext.extract(decision)
        p = p * 1000f + feature.value
      }
      p
    }

    override def toString: String = extractors.mkString(" < ")
  }

  import DecisionFeatures.extractors._
  val default: CSPView => DecisionOrdering =
    (csp: CSPView) => SeqFeatures(
      noChoice ::
        isSupport ::
        absLvl(csp, DetailedKnoblock) ::
        numOptions ::
        Nil)

}