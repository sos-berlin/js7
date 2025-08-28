package js7.base.utils

import js7.base.log.Logger
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict

object Assertions:

  private val logger = Logger[this.type]

  /** Like assertThat, but only if isStrict.
    */
  def assertIfStrict(predicate: sourcecode.Text[Boolean], clue: => String)
    (using fullName: sourcecode.FullName, loc: ScalaSourceLocation)
  : Unit =
    if isStrict then
      assertThat(predicate, clue)
    else if !predicate.value then
      logger.debug(s"🔥 Maybe an ERROR: ${toMessage("assertIfStrict", predicate, clue)}")

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
    (using fullName: sourcecode.FullName, loc: ScalaSourceLocation)
  : Unit =
    val c = clue
    throw new AssertionError(toMessage("assertThat", predicate, c))

  private def toMessage(method: String, predicate: sourcecode.Text[Boolean], clue: String)
    (using fullName: sourcecode.FullName, loc: ScalaSourceLocation)
  : String =
    s"$method(${predicate.source}) failed in ${fullName.value}, $loc${clue.nonEmpty ?? s", $clue"}"
