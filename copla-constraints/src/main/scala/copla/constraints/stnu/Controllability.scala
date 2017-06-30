package copla.constraints.stnu

final case class Controllability(id: Int)

object Controllability {
  val STN_CONSISTENCY         = Controllability(0)
  val WEAK_CONTROLLABILITY    = Controllability(1)
  val PSEUDO_CONTROLLABILITY  = Controllability(2)
  val DYNAMIC_CONTROLLABILITY = Controllability(3)
  val STRONG_CONTROLLABILITY  = Controllability(4)
}
