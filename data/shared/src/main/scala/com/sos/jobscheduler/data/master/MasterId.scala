package com.sos.jobscheduler.data.master

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops

/**
  * @author Joacim Zschimmer
  */
final case class MasterId private(string: String) extends GenericString
{
  def toUserId = UserId(string)
}

object MasterId extends GenericString.Companion[MasterId]
{
  def apply(o: String) = checked(o).orThrow

  override def checked(o: String) = UserId.checked(o) map (u ⇒ new MasterId(u.string))

  def fromUserId(userId: UserId) = MasterId(userId.string)
}
