package copla.planning.search

import copla.constraints.meta.CSPView
import copla.constraints.meta.decisions.Decision
import copla.lang.analysis.AbstractionHierarchyType
import copla.lang.model.core.FluentTemplate
import copla.planning.causality.support.SupportDecision
import copla.planning.events.PlanningHandler

object DecisionFeatures {

  type Feature = FeatureT[Float]
  type Extractor = ExtractorT[Float]

  case class FeatureT[A](name: String, value: A)


  trait ExtractorT[A] {
    def extractValue(decision: Decision)(implicit csp: CSPView): A
    final def extract(decision: Decision)(implicit csp: CSPView): FeatureT[A] =
      FeatureT(this.toString, extractValue(decision))
  }
  object ExtractorT {
    def map[A,B](self: ExtractorT[A], f: A => B): ExtractorT[B] = new ExtractorT[B] {
      override def extractValue(decision: Decision)(implicit csp: CSPView): B =
        f(self.extractValue(decision))
    }

    def named[A](self: ExtractorT[A], newName: String): ExtractorT[A] = new ExtractorT[A] {
      override def toString: String = newName
      override def extractValue(decision: Decision)(implicit csp: CSPView): A = self.extractValue(decision)
    }

  }

  implicit class ExtractorOps[A](val self: ExtractorT[A]) extends AnyVal {
    def map[B](f: A => B): ExtractorT[B] = ExtractorT.map(self, f)
    def withName(newName: String): ExtractorT[A] = ExtractorT.named(self, newName)
  }

  implicit class ExtratorBooleanOps(val self: ExtractorT[Boolean]) extends AnyVal {
    def trueFirst: Extractor = self.map(if(_) 0f else 1f)
    def falseFirst: Extractor = self.map(if(_) 1f else 0f)
  }
  implicit class ExtratorIntOps(val self: ExtractorT[Int]) extends AnyVal {
    def <=(threshold: Int): ExtractorT[Boolean] = self.map(_ <= threshold)
    def highestFirst: Extractor = self.map(v => - v.toFloat)
    def lowestFirst: Extractor = self.map(_.toFloat)
  }
  implicit class ExtratorOptionOps[A](val self: ExtractorT[Option[A]]) extends AnyVal {
    def mapValue[B](f: A => B): ExtractorT[Option[B]] = self.map(_.map(f))
  }
  implicit class ExtratorOptionIntOps(val self: ExtractorT[Option[Int]]) extends AnyVal {
    def lowestDefinedFirst: Extractor = self.map(opt => opt.map(_.toFloat).getOrElse(500f))
  }


  trait Encoder[A] {
    def encoderName: String
    def encodeValue(feature: A): Float
  }
  object Encoder {
    def instance[A](name: String, f: A => Float): Encoder[A] = new Encoder[A] {
      override def encodeValue(feature: A): Float = f(feature)
      override def encoderName: String = name
    }
  }



  val numOptionsExt: ExtractorT[Int] = new ExtractorT[Int] {
    override def extractValue(decision: Decision)(implicit csp: CSPView): Int =
      decision.numOptions
  }
  val isSupportDecision = new ExtractorT[Boolean] {
    override def extractValue(decision: Decision)(implicit csp: CSPView): Boolean = decision match {
      case x: SupportDecision => true
      case _ => false
    }
  }

  val supportDecisionFluent = new ExtractorT[Option[FluentTemplate]] {
    override def extractValue(decision: Decision)(implicit csp: CSPView): Option[FluentTemplate] = decision match {
      case x: SupportDecision => Some(x.supportVar.target.ref.fluent.template)
      case _ => None
    }
  }

  def abstractLevels(fluentLevels: FluentTemplate => Int) =
    supportDecisionFluent.mapValue(fluentLevels).lowestDefinedFirst



  object extractors {
    val noChoice = (numOptionsExt <= 1).trueFirst.withName("#options <= 1")
    val numOptions = numOptionsExt.lowestFirst.withName("#options")
    val isSupport = isSupportDecision.trueFirst.withName("is-support")
    val isNotSupport = isSupportDecision.falseFirst.withName("not-support")

    def absLvl(csp: CSPView, typ: AbstractionHierarchyType) = {
      val hier = copla.lang.analysis.abstractionHierarchy(
        csp.getHandler(classOf[PlanningHandler]).pb.anml,
        typ
      )
      supportDecisionFluent.mapValue(f => hier(f)).lowestDefinedFirst.withName(typ.toString)
    }
  }

}
