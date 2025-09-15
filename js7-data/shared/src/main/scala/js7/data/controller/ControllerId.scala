package js7.data.controller

import js7.base.auth.UserId
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichBoolean

/**
  * @author Joacim Zschimmer
  */
final case class ControllerId private(string: String) extends GenericString:

  // UserId is wrong when ControllerId is Undefined (only at Controller initialization)
  def unsafeUserId: UserId =
    UserId.unchecked(string)

  def toUserId: Checked[UserId] =
    check.flatMap: _ =>
      UserId.checked(string)

  def check: Checked[Unit] =
    (this != ControllerId.Undefined).orLeft:
      Problem.pure("ControllerId is undefined")

  override def toString = s"Controller:$string"


object ControllerId extends GenericString.Checked_[ControllerId]:
  val Undefined: ControllerId = unchecked("UNDEFINED-CONTROLLER-ID")

  protected def unchecked(string: String) =
    new ControllerId(string)

  override def checked(o: String): Checked[ControllerId] =
    UserId.checked(o).flatMap(u => super.checked(u.string))

  def fromUserId(userId: UserId): ControllerId =
    ControllerId(userId.string)
