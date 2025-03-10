package js7.data.lock

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemPath

/**
  * @author Joacim Zschimmer
  */
final case class LockPath private(string: String) extends UnsignedSimpleItemPath:
  protected type Self = LockPath

  val companion: LockPath.type = LockPath


object LockPath extends UnsignedSimpleItemPath.Companion[LockPath]:
  type Item = Lock

  protected def unchecked(string: String) = new LockPath(string)

  @javaApi
  def of(validName: String): LockPath =
    mayThrow(validName)
