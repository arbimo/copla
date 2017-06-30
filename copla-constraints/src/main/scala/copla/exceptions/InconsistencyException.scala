package copla.exceptions

class InconsistencyException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {

  def this() = this("", null)
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this("", cause)

}
