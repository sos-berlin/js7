package js7.base.utils

import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax.*

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
      val fn =
        if (isWindows)
          filename.value.lastIndexOf('\\') match {
            case -1 => filename.value
            case i => filename.value.drop(i + 1)
          }
        else
          filename.value
      throw new AssertionError(s"assertThat(${predicate.source}) failed in " +
        s"${fullName.value}, $fn:${line.value}${c.nonEmpty ?? s", $c"}")
    }
}
