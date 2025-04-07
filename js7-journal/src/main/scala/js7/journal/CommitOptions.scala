package js7.journal

import scala.concurrent.duration.*

final case class CommitOptions(
  transaction: Boolean = false,
  // commitLater: Option[FiniteDuration] = None, TODO combine commitLater with delay
  commitLater: Boolean = false /*For new Journaler only*/,
  delay: FiniteDuration = Duration.Zero,
  alreadyDelayed: FiniteDuration = Duration.Zero)


object CommitOptions:
  val default = new CommitOptions()
