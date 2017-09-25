package copla.constraints.meta

import copla.constraints.meta.constraints.{Change, UpdateDomain}
import copla.constraints.meta.domains.{Domain, IntervalDomain}
import copla.constraints.meta.stn.core.StnWithStructurals
import copla.constraints.meta.stn.variables.{TemporalDelay, Timepoint}
import copla.constraints.meta.types.TypesStore
import copla.constraints.meta.variables.{IntVariable, VariableStore}
import copla.constraints.stn.STN

trait CSPView {

  def dom(tp: Timepoint): IntervalDomain

  def dom(d: TemporalDelay): IntervalDomain

  def dom(v: IntVariable): Domain

  def types: TypesStore
  def varStore: VariableStore
  def constraints: ConstraintStore
  def temporalOrigin: Timepoint

  def getHandler[T](clazz: Class[T]): T

  def +(change: Change): CSPView        = new CSPProxy(this, change)
  def ++(changes: Seq[Change]): CSPView = changes.foldLeft(this)(_ + _)
}

class CSPProxy(view: CSPView, change: Change) extends CSPView {

  override def dom(tp: Timepoint): IntervalDomain = view.dom(tp)

  override def dom(d: TemporalDelay): IntervalDomain = view.dom(d)

  override def dom(v: IntVariable): Domain = change match {
    case UpdateDomain(`v`, domain) => domain
    case _                         => view.dom(v)
  }

  override def varStore                     = view.varStore
  override def types                        = view.types
  override def constraints: ConstraintStore = view.constraints
  def temporalOrigin: Timepoint             = view.temporalOrigin

  override def getHandler[T](clazz: Class[T]): T = view.getHandler(clazz)
}
