package js7.data.lock

import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class LockName private(string: String) extends GenericString
{
  def companion = LockName

  override def toString = s"Lock:$string"   // Used in LockState error message
}

object LockName extends GenericString.NameValidating[LockName]
{
  protected def unchecked(string: String) = new LockName(string)

  @javaApi
  def of(validName: String) = apply(validName)
}
