package js7.data

import js7.base.io.process.{ProcessSignal, ReturnCode}

package object value:
  val missingValue: Value = MissingValue

  type NamedValues = Map[String, Value]

  object NamedValues:
    val empty = Map.empty[String, Value]

    def apply(kv: (String, Value)*): NamedValues =
      Map(kv*)

    def rc(number: Int): NamedValues =
      Map(Value.ShellReturnCode -> NumberValue(number))

    def rc(returnCode: ReturnCode): NamedValues =
      NamedValues.rc(returnCode.number)

    def rc(signal: ProcessSignal): NamedValues =
      NamedValues.rc(ReturnCode(signal))
