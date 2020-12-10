package js7.data.lock

import js7.base.problem.Problem

sealed trait LockRefusal {
  def toProblem(lockId: LockId): Problem
}

object LockRefusal {

  case object AlreadyAcquiredByThisOrder extends LockRefusal {
    def toProblem(lockId: LockId) =
      Problem(s"$lockId has already been acquired by this order")
  }

  case object IsInUse extends LockRefusal {
    def toProblem(lockId: LockId) =
      Problem(s"$lockId is in use")
  }

  final case class LimitReached(limit: Int) extends LockRefusal {
    def toProblem(lockId: LockId) =
      Problem(s"$lockId limit=$limit reached")
  }

  case object UnknownReleasingOrderError extends LockRefusal {
    def toProblem(lockId: LockId) =
      Problem(s"$lockId has not been acquired by this order")
  }
}
