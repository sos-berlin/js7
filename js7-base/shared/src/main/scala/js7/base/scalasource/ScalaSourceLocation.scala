package js7.base.scalasource

import js7.base.utils.ScalaUtils.syntax.RichString
import js7.base.utils.Tests.isIntelliJIdea
import scala.quoted.{Expr, Quotes, quotes}
import sourcecode.{SourceCompanion, SourceValue}

final class ScalaSourceLocation(val value: (String, Int)) extends SourceValue[(String, Int)]:

  def filename: String =
    value._1

  def line: Int =
    value._2

  def asString: String =
    s"$filename:$line"

  override def toString =
    if isIntelliJIdea then
      // IntelliJ Idea highlights this as a clickable source code reference
      // https://stackoverflow.com/a/14913309
      s".($asString)"
    else
      asString


object ScalaSourceLocation
  extends SourceCompanion[(String, Int), ScalaSourceLocation](new ScalaSourceLocation(_)):

  def apply(filename: String, line: Int): ScalaSourceLocation =
    new ScalaSourceLocation((filename, line))

  inline implicit def generate: ScalaSourceLocation =
    ${ ScalaSourceLocationMacros.fileLocationMacro }


  /** Strip source code lines and combine them to a single, shorted line. */
  def sourceCodeToString(text: sourcecode.Text[?]): String =
    text.source.split('\n').map(_.trim).mkString("⏎").truncateWithEllipsis(80, quote = true)


private object ScalaSourceLocationMacros:
  def fileLocationMacro(using Quotes): Expr[ScalaSourceLocation] =
    val position = quotes.reflect.Position.ofMacroExpansion
    val filename = position.sourceFile.name
    val line = position.startLine + 1
    '{ ScalaSourceLocation(${ Expr(filename) }, ${ Expr(line) }) }
