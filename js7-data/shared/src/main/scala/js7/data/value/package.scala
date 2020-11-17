package js7.data

import js7.base.process.ProcessSignal
import js7.data.job.ReturnCode

package object value
{
  type NamedValues = Map[String, Value]

  object NamedValues
  {
    val empty = Map.empty[String, Value]

    def rc(number: Int): NamedValues =
      Map("returnCode" -> NumericValue(number))

    def rc(returnCode: ReturnCode): NamedValues =
      NamedValues.rc(returnCode.number)

    def rc(signal: ProcessSignal): NamedValues =
      NamedValues.rc(ReturnCode(signal))
  }
}
