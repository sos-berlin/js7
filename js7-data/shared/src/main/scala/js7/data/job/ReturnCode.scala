package js7.data.job

import js7.base.annotation.javaApi
import js7.base.generic.GenericInt
import js7.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode(number: Int) extends GenericInt
{
  def isSuccess = number == 0

  def isProcessSignal: Boolean =
    number > 128 && number < 256
}

object ReturnCode extends GenericInt.Companion[ReturnCode]
{
  val Success = new ReturnCode(0)
  val StandardFailure = new ReturnCode(1)

  def apply(o: Boolean): ReturnCode =
    if (o) Success else StandardFailure

  def apply(signal: ProcessSignal) =
    new ReturnCode(128 + signal.number)

  @javaApi
  def fromBoolean(o: Boolean): ReturnCode =
    apply(o)
}
