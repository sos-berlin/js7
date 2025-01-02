package js7.base.utils

import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict

object Assertions:

  /** Like assertThat, but only if isStrict.
    */
  inline def strictly(inline predicate: sourcecode.Text[Boolean])
    (using
      inline fullName: sourcecode.FullName,
      inline filename: sourcecode.FileName,
      inline line: sourcecode.Line)
  : Unit =
    if isStrict then
      assertThat(predicate)

  def assertThat(predicate: sourcecode.Text[Boolean])
    (using
      fullName: sourcecode.FullName,
      filename: sourcecode.FileName,
      line: sourcecode.Line)
  : Unit =
    assertThat(predicate, "")

  def assertThat(predicate: sourcecode.Text[Boolean], clue: => String)
    (using
      fullName: sourcecode.FullName,
      filename: sourcecode.FileName,
      line: sourcecode.Line)
  : Unit =
    if !predicate.value then
      val c = clue
      val fn =
        if isWindows then
          filename.value.lastIndexOf('\\') match
            case -1 => filename.value
            case i => filename.value.drop(i + 1)
        else
          filename.value
      throw new AssertionError(s"assertThat(${predicate.source}) failed in " +
        s"${fullName.value}, $fn:${line.value}${c.nonEmpty ?? s", $c"}")
