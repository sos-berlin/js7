package js7.data.controller

import js7.base.auth.UserId
import js7.base.generic.GenericString
import js7.base.problem.Checked

/**
  * @author Joacim Zschimmer
  */
final case class ControllerId private(string: String) extends GenericString:
  def toUserId: UserId = UserId(string)

  override def toString = s"Controller:$string"


object ControllerId extends GenericString.Checked_[ControllerId]:

  protected def unchecked(string: String) =
    new ControllerId(string)

  override def checked(o: String): Checked[ControllerId] =
    UserId.checked(o).flatMap(u => super.checked(u.string))

  def fromUserId(userId: UserId): ControllerId =
    ControllerId(userId.string)
