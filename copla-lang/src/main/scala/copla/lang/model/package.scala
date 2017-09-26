package copla.lang

import copla.lang.model.core.{InnerScope, RootScope, SimpleTPRef}
import copla.lang.parsing.anml.Parser

package object model {

  val reservedPrefix       = "__"
  private[this] var nextID = 0
  def defaultId(): String  = reservedPrefix + { nextID += 1; nextID - 1 }

}
