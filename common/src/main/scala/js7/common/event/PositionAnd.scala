package js7.common.event

/**
  * @author Joacim Zschimmer
  */
final case class PositionAnd[@specialized(Long/*EventId*/) A](position: Long, value: A)
