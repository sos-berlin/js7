package js7.core.api

import js7.data.workflow.WorkflowPrinter

object Api
{
  def quoteString(string: String): String = {
    val sb = new StringBuilder(string.length)
    WorkflowPrinter.appendQuoted(sb, string)
    sb.toString
  }
}
