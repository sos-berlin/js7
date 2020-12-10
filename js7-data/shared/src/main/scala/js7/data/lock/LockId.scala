package js7.data.lock

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class LockId private(string: String) extends GenericString
{
  def companion = LockId

  override def toString = s"Lock:$string"   // Used in LockState error message
}

object LockId extends GenericString.NameValidating[LockId]
{
  protected def unchecked(string: String) = new LockId(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
