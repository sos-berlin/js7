package js7.data.controller

import js7.base.auth.UserId
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class ControllerId private(string: String) extends GenericString:
  def toUserId = UserId(string)

  override def toString = s"ControllerId:$string"


object ControllerId extends GenericString.Checked_[ControllerId]:
  protected def unchecked(string: String) = new ControllerId(string)

  override def checked(o: String) = UserId.checked(o).flatMap(u => super.checked(u.string))

  def fromUserId(userId: UserId) = ControllerId(userId.string)
