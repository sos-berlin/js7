package com.sos.jobscheduler.base.utils

object Assertions
{
  def assertThat(predicate: sourcecode.Text[Boolean])
    (implicit fullName: sourcecode.FullName, file: sourcecode.File, line: sourcecode.Line)
  : Unit =
    if (!predicate.value) {
      throw new java.lang.AssertionError(s"assertThat(${predicate.source}) failed in ${fullName.value}, ${file.value}:${line.value}")
    }
}
