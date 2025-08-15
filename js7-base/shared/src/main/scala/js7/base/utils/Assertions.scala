package js7.base.utils

import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict

object Assertions:

  private val logger = Logger[this.type]

  /** Like assertThat, but only if isStrict.
    */
  def assertIfStrict(predicate: sourcecode.Text[Boolean], clue: => String)
    (using sourcecode.FullName, ScalaSourceLocation)
  : Unit =
    if isStrict || logger.isDebugEnabled then
      try
        assertThat(predicate, clue)
      catch
        case e: AssertionError if !isStrict =>
          logger.debug(s"🔥 possible ERROR (suppressed) $e")

  def assertThat(predicate: sourcecode.Text[Boolean])
    (using sourcecode.FullName, ScalaSourceLocation)
  : Unit =
    assertThat(predicate, "")

  def assertThat(predicate: sourcecode.Text[Boolean], clue: => String)
    (using sourcecode.FullName, ScalaSourceLocation)
  : Unit =
    if !predicate.value then
      fail(predicate, clue)

  private def fail(predicate: sourcecode.Text[Boolean], clue: => String)
    (using
      fullName: sourcecode.FullName,
      loc: ScalaSourceLocation)
  : Unit =
    val c = clue
    throw new AssertionError(s"assertThat(${predicate.source}) failed in " +
      s"${fullName.value}, $loc${c.nonEmpty ?? s", $c"}")
