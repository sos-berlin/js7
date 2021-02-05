package js7.base.io.process

import js7.base.annotation.javaApi
import js7.base.generic.GenericInt

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode private(number: Int) extends GenericInt
{
  def isSuccess = number == 0

  def isProcessSignal: Boolean =
    number > 128 && number < 256
}

object ReturnCode extends GenericInt.Companion[ReturnCode]
{
  private val predefined = (0 to 255).map(new ReturnCode(_)).toArray

  val Success = predefined(0)
  val StandardFailure = predefined(1)

  def apply(number: Int): ReturnCode =
    if (predefined isDefinedAt number)
      predefined(number)
    else
      new ReturnCode(number)

  def apply(o: Boolean): ReturnCode =
    if (o) Success else StandardFailure

  def apply(signal: ProcessSignal): ReturnCode =
    apply(128 + signal.number)

  @javaApi
  def fromBoolean(o: Boolean): ReturnCode =
    apply(o)
}
