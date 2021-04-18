package js7.data.lock

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemId

/**
  * @author Joacim Zschimmer
  */
final case class LockId private(string: String) extends UnsignedSimpleItemId
{
  protected type Self = LockId

  val companion = LockId
}

object LockId extends UnsignedSimpleItemId.Companion[LockId]
{
  def itemTypeName = Lock.typeName

  protected def unchecked(string: String) = new LockId(string)

  @javaApi
  def of(validName: String): LockId =
    apply(validName)
}
