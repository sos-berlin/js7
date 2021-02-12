package js7.executor.task

import monix.reactive.Observer

/**
  * @author Joacim Zschimmer
  */
final case class StdChannels(
  charBufferSize: Int,
  out: Observer[String],
  err: Observer[String])
