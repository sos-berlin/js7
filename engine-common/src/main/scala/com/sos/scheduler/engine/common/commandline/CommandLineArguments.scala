package com.sos.scheduler.engine.common.commandline

import com.sos.scheduler.engine.common.commandline.CommandLineArguments.{Argument, NameOnly}
import java.util.NoSuchElementException
import scala.collection.{immutable, mutable}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class CommandLineArguments private(val argMap: mutable.LinkedHashMap[String, List[Argument]]) {

  private val unusedArguments = new mutable.LinkedHashMap[String, List[Argument]] ++ argMap

  def boolean(name: String): Boolean =
    arguments(name) match {
      case Nil ⇒ false
      case (_: NameOnly) :: Nil ⇒ true
      case _ ⇒ throw new IllegalArgumentException(s"Multiple options -$name")
    }

  def int(name: String) = asConverted(name) { _.toInt }

  def string(name: String): String = asConverted(name)(identity)

  def nameLessValues: List[String] = arguments("") map { _.value }

  def asConverted[A](name: String)(convert: String ⇒ A): A =
    asConvertedOption(name)(convert) getOrElse { throw new NoSuchElementException(if (name.nonEmpty) s"Missing option $name" else s"Missing argument") }

  def asConvertedOption[A](name: String)(convert: String ⇒ A): Option[A] =
    asConvertedList(name)(convert) match {
      case Nil ⇒ None
      case value :: Nil ⇒ Some(value)
      case _ ⇒ throw new IllegalArgumentException(s"Only one value for -$name= is possible")
    }

  def asConvertedList[A](name: String)(convert: String ⇒ A): List[A] =
    arguments(name) map { o ⇒
      try convert(o.value)
      catch { case NonFatal(t) ⇒ throw new IllegalArgumentException(s"Error in -$name=: $t", t) }
    }

  private def arguments(name: String): List[Argument] = {
    unusedArguments -= name
    argMap(name)
  }

  def requireNoMoreArguments(): Unit = {
    if (unusedArguments.nonEmpty)
      throw new IllegalArgumentException("Unknown arguments: " + unusedArguments.values.flatten.mkString(" "))
  }

//  def string(name: String): String =
//    arg(name) match {
//      case value :: Nil ⇒ value.value
//      case Nil ⇒ throw new NoSuchElementException(if (name.nonEmpty) s"Missing option -$name" else s"Missing argument")
//      case o ⇒ throw new IllegalArgumentException(s"Only one value for -$name= is possible")
//    }
}

object CommandLineArguments {
  private val OptionWithValueRegex = "(-[^=]+=)(.*)".r

  def apply(args: Seq[String]): CommandLineArguments = {
    val m = new mutable.LinkedHashMap[String, immutable.List[Argument]] {
      override def default(name: String) = Nil//throw new IllegalArgumentException(if (name.nonEmpty) s"Missing option -$name" else s"Missing argument")
    }
    for (a ← parseArgs(args)) {
      m.get(a.name) match {
        case None ⇒ m += a.name → List(a)
        case Some(seq) ⇒ m(a.name) = seq :+ a
      }
    }
    new CommandLineArguments(m)
  }

  def parseArgs(args: Seq[String]): List[Argument] = (args map toArgument).toList

  def toArgument(string: String): Argument =
    string match {
      case OptionWithValueRegex(name, value) ⇒ NameValue(name, value)
      case o if string startsWith "-" ⇒ NameOnly(o)
      case o ⇒ ValueOnly(o)
    }

  trait Argument {
    def name: String
    def value: String
  }

  final case class NameOnly(name: String) extends Argument {
    override def toString = s"-$name"
    def value = throw new UnsupportedOperationException("Option -$name has no value")
  }

  final case class NameValue(name: String, value: String) extends Argument {
    def toInt =
      try value.toInt
      catch { case e: NumberFormatException ⇒ throw new IllegalArgumentException(s"Number expected: $toString")}

    override def toString = s"-$name=$value"
  }

  case class ValueOnly(value: String) extends Argument {
    def name = ""
  }
}
