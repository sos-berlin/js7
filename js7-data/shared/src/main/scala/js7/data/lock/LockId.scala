package js7.data.lock

import js7.base.annotation.javaApi
import js7.data.item.SimpleItemId

/**
  * @author Joacim Zschimmer
  */
final case class LockId private(string: String) extends SimpleItemId
{
  protected type Self = LockId

  val companion = LockId

  override def toString = s"Lock:$string"   // Used in LockState error message
}

object LockId extends SimpleItemId.Companion[LockId]
{
  def itemTypeName = Lock.typeName

  protected def unchecked(string: String) = new LockId(string)

  @javaApi
  def of(validName: String): LockId =
    apply(validName)
}
