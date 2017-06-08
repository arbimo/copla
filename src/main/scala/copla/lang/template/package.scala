package copla.lang

import rx.Rx

import scala.collection.mutable

object template {
  import copla.Lang._

  case class TBefore(from: TPRef, to: TPRef) extends Elem with ActionElem {

    override def toString: String = s"$from <= $to"
  }
  case class TDeadline(tp: TPRef, deadline: Int) extends Elem with ActionElem {
    override def toString: String = s"$tp <= $deadline"
  }
  case class Delay(from: TPRef, to: TPRef) {
    def <=(dur: Int) = to <= from + dur
    def <(dur: Int)  = to < from + dur
    def >=(dur: Int) = to >= from + dur
    def >(dur: Int)  = to > from + dur
    def +(time: Int) = Delay(from, to + 1)
    def -(time: Int) = Delay(from, to - 1)
  }
  class TPRef(val id: Symbol, val delay: Int = 0)(implicit val ctx: Context) {
    val tp: TTimepoint = ctx.findTimepoint(id) match {
      case Some(timepoint) => timepoint
      case None            => sys.error(s"Could not identify the timepoint: $id")
    }

    override def toString = id.toString()

    def +(delay: Int) = new TPRef(id, this.delay + delay)
    def -(delay: Int) = new TPRef(id, this.delay - delay)

    def <=(other: TPRef) = TBefore(this, other)
    def <(other: TPRef)  = TBefore(this, other + 1)
    def >=(other: TPRef) = other <= this
    def >(other: TPRef)  = other < this
  }
  implicit def symbol2tpref(s: Symbol)(implicit ctx: Context): TPRef = new TPRef(s)
  implicit def int2tpref(time: Int)(implicit ctx: Context)           = ctx.module.start + time

  class TTimepoint(val id: Symbol)(implicit ctx: Context) {
    ctx.localTimepoints += this
    val global: Option[Timepoint] =
      if (ctx.isTemplate) None
      else Some(new Timepoint(id))

    override def toString = id.toString()
  }
  class Timepoint(val id: Symbol)

  def timepoint(id: Symbol)(implicit ctx: Context) = {
    new TTimepoint(id) // declare new timepoint in context
    new TPRef(id)      // return reference on this timepoint
  }

  trait ActionElem

  case class Action(name: Symbol, parameters: Seq[Arg])(implicit mod: Module)
      extends Elem
      with Context {
    implicit val ctx: Context = this
    module.add(this)
    for (param <- parameters)
      ctx.localVariables += TVariable(param.name, param.typ)

    override def isTemplate                     = true
    override def parentContext: Option[Context] = Some(mod)

    val start    = timepoint('start)
    val end      = timepoint('end)
    val duration = Delay(start, end) + 1

    val elems = mutable.ArrayBuffer[ActionElem]()

    def core(items: ActionElem*) {
      elems ++= items; println(items.mkString("\n"))
    }
  }

}
