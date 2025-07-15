package js7.base.time

import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{Deadline, FiniteDuration}

/** Like Scala's Deadline, but with pretty toString. */
final class Since:
  private val since = Deadline.now

  def elapsed: FiniteDuration =
    since.elapsed

  override def toString =
    since.elapsed.pretty