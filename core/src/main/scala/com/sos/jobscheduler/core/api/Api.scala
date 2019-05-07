package com.sos.jobscheduler.core.api

import com.sos.jobscheduler.data.workflow.WorkflowPrinter

object Api
{
  def quoteString(string: String): String = {
    val sb = new StringBuilder(string.length)
    WorkflowPrinter.appendQuoted(sb, string)
    sb.toString
  }
}
