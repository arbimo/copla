package copla.collection.mutable

import scala.collection.{mutable => sm}

/**
  * A buffer implementation that invokes the  onUpdate function
  * on each update to the buffer's content
  */
class CheckedBuffer[A](elements: Seq[A], onUpdate: Seq[A] => Unit) extends sm.Buffer[A] {
  val inner: sm.Buffer[A] = sm.Buffer(elements: _*)

  override def apply(n: Int) = inner(n)

  override def update(n: Int, newelem: A) = {
    inner.update(n, newelem)
    onUpdate(inner)
  }

  override def length = inner.length

  override def +=(elem: A) = {
    inner += elem
    onUpdate(inner)
    this
  }

  override def clear() = {
    inner.clear()
    onUpdate(inner)
  }

  override def +=:(elem: A) = {
    elem +=: inner
    onUpdate(inner)
    this
  }

  override def insertAll(n: Int, elems: Traversable[A]) = {
    inner.insertAll(n, elems)
    onUpdate(inner)
  }

  override def remove(n: Int) = {
    val ret = inner.remove(n)
    onUpdate(inner)
    ret
  }

  override def iterator = inner.iterator
}

object CheckedBuffer {
  def apply[A](onUpdate: Seq[A] => Unit): CheckedBuffer[A] = new CheckedBuffer(Nil, onUpdate)
}