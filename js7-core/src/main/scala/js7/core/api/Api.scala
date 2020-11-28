package js7.core.api

import js7.data.value.ValuePrinter

object Api
{
  def quoteString(string: String): String =
    ValuePrinter.quoteString(string)
}
