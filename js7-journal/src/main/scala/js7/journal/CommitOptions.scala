package js7.journal

import scala.concurrent.duration.*

final case class CommitOptions(
  transaction: Boolean = false,
  // commitLater: Option[FiniteDuration] = None, TODO combine commitLater with delay
  commitLater: Boolean = false,
  delay: FiniteDuration = Duration.Zero,
  alreadyDelayed: FiniteDuration = Duration.Zero)


object CommitOptions:

  val default: CommitOptions =
    CommitOptions()

  val Transaction: CommitOptions =
    CommitOptions(transaction = true)

  val CommitLater: CommitOptions =
    CommitOptions(commitLater = true)
