package js7.base.process

/**
  * @author Joacim Zschimmer
  */
sealed abstract class ProcessSignal(val value: Int)

object ProcessSignal {
  case object SIGTERM extends ProcessSignal(15)
  case object SIGKILL extends ProcessSignal(9)
}
