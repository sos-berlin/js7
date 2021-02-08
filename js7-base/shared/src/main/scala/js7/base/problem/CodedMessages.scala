package js7.base.problem

import java.lang.Character.{isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.atomic.AtomicAny
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object CodedMessages
{
  // Yes, a global static variable, to be intialized by main()
  private val _codeToPattern = AtomicAny[ProblemCode => Option[String]](noCodeToPattern)

  implicit val problemCodeToPattern: ProblemCode => Option[String] = codeToPattern

  def codeToPattern: ProblemCode => Option[String] =
    _codeToPattern.get()

  def codeToPattern_=(f: ProblemCode => Option[String]): Unit =
    _codeToPattern := f

  private def noCodeToPattern(code: ProblemCode): Option[String] =
    None

  def problemCodeToMessage(code: ProblemCode, arguments: Map[String, String]) =
    codeToPattern(code) match {
      case None => code.string + unusedArgumentsToString(arguments)
      case Some(pattern) => patternToMessage(pattern, arguments)
    }

  private[problem] def patternToMessage(pattern: String, arguments: Map[String, String]) = {
    val used = mutable.Set.empty[String]
    val it = pattern.iterator.buffered
    val sb = new StringBuilder(pattern.length)
    while (it.hasNext) {
      val c = it.next()
      if (c == '$' && it.headOption.exists(isUnicodeIdentifierStart)) {
        val keyBuilder = new StringBuilder
        keyBuilder += it.next()
        while (it.headOption.exists(isUnicodeIdentifierPart)) keyBuilder += it.next()
        val key = keyBuilder.toString
        arguments.get(key) match {
          case None =>
            sb += '$'
            sb ++= key
          case Some(value) =>
            sb ++= value
            used += key
        }
      } else
        sb += c
    }
    val unused = arguments.keySet -- used
    if (unused.nonEmpty) {
      if (sb contains ' ') sb.append(' ')
      sb ++= unusedArgumentsToString(arguments.view.filterKeys(unused).toMap)
    }
    sb.toString
  }

  private def unusedArgumentsToString(arguments: Map[String, String]): String =
    arguments.nonEmpty ??
      arguments.map { case (k, v) => s"$k=$v" } .mkString("(", ", ", ")")
}
