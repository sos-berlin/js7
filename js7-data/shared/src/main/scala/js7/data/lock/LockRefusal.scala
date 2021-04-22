package js7.data.lock

import js7.base.problem.Problem

sealed trait LockRefusal {
  def toProblem(lockId: LockPath): Problem
}

object LockRefusal {

  case object AlreadyAcquiredByThisOrder extends LockRefusal {
    def toProblem(lockId: LockPath) =
      Problem(s"$lockId has already been acquired by this order")
  }

  case object IsInUse extends LockRefusal {
    def toProblem(lockId: LockPath) =
      Problem(s"$lockId is in use")
  }

  final case class LimitReached(limit: Int, count: Int, requestedCount: Int) extends LockRefusal {
    def toProblem(lockId: LockPath) =
      Problem(s"$lockId: $count+$requestedCount would exceed limit=$limit")
  }

  final case class InvalidCount(count: Int) extends LockRefusal {
    def toProblem(lockId: LockPath) =
      Problem(s"$lockId: Invalid count=$count requested")
  }

  case object UnknownReleasingOrderError extends LockRefusal {
    def toProblem(lockId: LockPath) =
      Problem(s"$lockId has not been acquired by this order")
  }
}
