package js7.data.lock

import js7.base.problem.Problem

sealed trait LockRefusal:
  def toProblem(lockPath: LockPath): Problem


object LockRefusal:

  sealed trait NotAvailable extends LockRefusal

  case object IsInUse extends NotAvailable:
    def toProblem(lockPath: LockPath) =
      Problem(s"$lockPath is in use")

  final case class LimitReached(limit: Int, count: Int, alreadyRequiredCount: Int)
  extends NotAvailable:
    def toProblem(lockPath: LockPath) =
      Problem(
        s"$lockPath: count=$count plus alreadyAcquired=$alreadyRequiredCount would exceed limit=$limit")

  case object AlreadyAcquiredByThisOrder extends LockRefusal:
    def toProblem(lockPath: LockPath) =
      Problem(s"$lockPath has already been acquired by this order")

  final case class InvalidCount(count: Int) extends LockRefusal:
    def toProblem(lockPath: LockPath) =
      Problem(s"$lockPath: Invalid count=$count requested")

  case object UnknownReleasingOrderError extends LockRefusal:
    def toProblem(lockPath: LockPath) =
      Problem(s"$lockPath has not been acquired by this order")
