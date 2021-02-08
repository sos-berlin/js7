package js7.data

import js7.base.io.process.{ProcessSignal, ReturnCode}

package object value
{
  type NamedValues = Map[String, Value]

  object NamedValues
  {
    val empty = Map.empty[String, Value]

    def apply(kv: (String, Value)*): NamedValues =
      Map(kv: _*)

    def rc(number: Int): NamedValues =
      Map("returnCode" -> NumberValue(number))

    def rc(returnCode: ReturnCode): NamedValues =
      NamedValues.rc(returnCode.number)

    def rc(signal: ProcessSignal): NamedValues =
      NamedValues.rc(ReturnCode(signal))
  }
}
