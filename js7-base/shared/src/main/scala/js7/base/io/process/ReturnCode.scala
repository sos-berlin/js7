package js7.base.io.process

import js7.base.annotation.javaApi
import js7.base.generic.GenericInt
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode private(number: Int) extends GenericInt:
  def isSuccess = number == 0

  def isProcessSignal: Boolean =
    number > 128 && number < 256

  def pretty(isWindows: Boolean) =
    if isWindows || !isProcessSignal then
      toString
    else
      number - 128 match
        case SIGTERM.number => s"ReturnCode($number/SIGTERM)"
        case SIGKILL.number => s"ReturnCode($number/SIGKILL)"
        case signal => s"ReturnCode($number=128+$signal)"

object ReturnCode extends GenericInt.Companion[ReturnCode]:
  private val predefined = (0 to 255).map(new ReturnCode(_)).toArray

  val Success = predefined(0)
  val StandardFailure = predefined(1)

  def apply(number: Int): ReturnCode =
    if predefined isDefinedAt number then
      predefined(number)
    else
      new ReturnCode(number)

  def apply(o: Boolean): ReturnCode =
    if o then Success else StandardFailure

  def apply(signal: ProcessSignal): ReturnCode =
    apply(128 + signal.number)

  @javaApi
  def fromBoolean(o: Boolean): ReturnCode =
    apply(o)
