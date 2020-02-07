package com.sos.jobscheduler.base.utils

object Assertions
{
  def assertThat(predicate: sourcecode.Text[Boolean])
    (implicit fullName: sourcecode.FullName, filename: sourcecode.FileName, line: sourcecode.Line)
  : Unit =
    assertThat(predicate, "")

  def assertThat(predicate: sourcecode.Text[Boolean], clue: => String)
    (implicit fullName: sourcecode.FullName, filename: sourcecode.FileName, line: sourcecode.Line)
  : Unit =
    if (!predicate.value) {
      val c = clue
      throw new java.lang.AssertionError(s"assertThat(${predicate.source}) failed in " +
        s"${fullName.value}, ${filename.value}:${line.value}${if (c.isEmpty) "" else s", $clue"}")
    }
}
