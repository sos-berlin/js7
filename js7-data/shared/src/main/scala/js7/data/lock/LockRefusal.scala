package js7.data.lock

import js7.base.problem.Problem

sealed trait LockRefusal {
  def toProblem(lockName: LockName): Problem
}

object LockRefusal {

  case object AlreadyAcquiredByThisOrder extends LockRefusal {
    def toProblem(lockName: LockName) = Problem(s"$lockName has already been acquired by this order")
  }

  case object IsInUse extends LockRefusal {
    def toProblem(lockName: LockName) = Problem(s"$lockName is in use")
  }

  final case class LimitReached(limit: Int) extends LockRefusal {
    def toProblem(lockName: LockName) = Problem(s"$lockName limit=$limit reached")
  }

  case object UnknownReleasingOrderError extends LockRefusal {
    def toProblem(lockName: LockName) = Problem(s"$lockName has not been acquired by this order")
  }
}
