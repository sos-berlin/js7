package js7.data.master

import js7.base.auth.UserId
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class MasterId private(string: String) extends GenericString
{
  def toUserId = UserId(string)
}

object MasterId extends GenericString.Checked_[MasterId]
{
  def unchecked(string: String) = new MasterId(string)

  override def checked(o: String) = UserId.checked(o) flatMap (u => super.checked(u.string))

  def fromUserId(userId: UserId) = MasterId(userId.string)
}
