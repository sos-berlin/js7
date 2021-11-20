package js7.journal

import scala.concurrent.duration._

final case class CommitOptions(
  transaction: Boolean = false,
  delay: FiniteDuration = Duration.Zero,
  alreadyDelayed: FiniteDuration = Duration.Zero)

object CommitOptions
{
  val default = new CommitOptions()
}
