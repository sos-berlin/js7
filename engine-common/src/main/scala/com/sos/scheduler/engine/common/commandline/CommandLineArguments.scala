package com.sos.scheduler.engine.common.commandline

import com.sos.scheduler.engine.common.commandline.CommandLineArguments.{Argument, NameOnly}
import java.util.NoSuchElementException
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class CommandLineArguments private(val argMap: mutable.LinkedHashMap[String, Vector[Argument]]) {

  private val unusedArguments = new mutable.LinkedHashMap[String, Vector[Argument]] ++ argMap
  private val unusedNamelessArguments = new mutable.HashSet[Int]() ++ arguments("").indices

  def boolean(name: String): Boolean =
    arguments(name) match {
      case Vector() ⇒ false
      case Vector(_: NameOnly) ⇒ true
      case _ ⇒ throw new IllegalArgumentException(s"Multiple options -$name")
    }

  def int(name: String) = asConverted(name) { _.toInt }

  def getString(name: String): Option[String] = asConvertedOption(name)(identity)

  def string(name: String): String = asConverted(name)(identity)

  def namelessValue(index: Int): String = {
    unusedNamelessArguments -= index
    val a = arguments("")
    if (index >= a.size) throw new NoSuchElementException(s"To few nameless arguments: argument #${index+1} expected")
    a(index).value
  }

  lazy val namelessValues: Vector[String] = {
    unusedNamelessArguments.clear()
    arguments("") map { _.value }
  }

  def asConverted[A](name: String)(convert: String ⇒ A): A =
    asConvertedOption(name)(convert) getOrElse { throw new NoSuchElementException(if (name.nonEmpty) s"Missing option $name" else s"Missing argument") }

  def asConvertedOption[A](name: String)(convert: String ⇒ A): Option[A] =
    asConvertedList(name)(convert) match {
      case Vector() ⇒ None
      case Vector(value) ⇒ Some(value)
      case _ ⇒ throw new IllegalArgumentException(s"Only one value for -$name= is possible")
    }

  def asConvertedList[A](name: String)(convert: String ⇒ A): Vector[A] =
    arguments(name) map { o ⇒
      try convert(o.value)
      catch { case NonFatal(t) ⇒ throw new IllegalArgumentException(s"Error in -$name=: $t", t) }
    }

  private def arguments(name: String): Vector[Argument] = {
    unusedArguments -= name
    argMap(name)
  }

  def requireNoMoreArguments(): Unit = {
    if (unusedArguments.nonEmpty || unusedNamelessArguments.nonEmpty)
      throw new IllegalArgumentException("Unknown arguments: " + (unusedNamelessArguments map { o ⇒ s"#${o+1}" }) .mkString(" ") +
        unusedArguments.values.flatten.mkString(" "))
  }

//  def string(name: String): String =
//    arg(name) match {
//      case value :: Nil ⇒ value.value
//      case Nil ⇒ throw new NoSuchElementException(if (name.nonEmpty) s"Missing option -$name" else s"Missing argument")
//      case o ⇒ throw new IllegalArgumentException(s"Only one value for -$name= is possible")
//    }
}

object CommandLineArguments {
  private val OptionWithValueRegex = "(?s)(-[^=]+=)(.*)".r   // "(?s)" to match multi-line arguments

  def apply(args: Seq[String]): CommandLineArguments = {
    val m = new mutable.LinkedHashMap[String, Vector[Argument]] {
      override def default(name: String) = Vector()//throw new IllegalArgumentException(if (name.nonEmpty) s"Missing option -$name" else s"Missing argument")
    }
    for (a ← parseArgs(args)) {
      m.get(a.name) match {
        case None ⇒ m += a.name → Vector(a)
        case Some(seq) ⇒ m(a.name) = seq :+ a
      }
    }
    new CommandLineArguments(m)
  }

  private def parseArgs(args: Seq[String]): List[Argument] = (args map toArgument).toList

  private def toArgument(string: String): Argument =
    string match {
      case OptionWithValueRegex(name, value) ⇒ NameValue(name, value)
      case "-" ⇒ ValueOnly("-")
      case o if string startsWith "-" ⇒ NameOnly(o)
      case o ⇒ ValueOnly(o)
    }

  def parse[A](args: Seq[String])(convert: CommandLineArguments ⇒ A): A = {
    val arguments = CommandLineArguments(args)
    val result = convert(arguments)
    arguments.requireNoMoreArguments()
    result
  }

  sealed trait Argument {
    def name: String
    def value: String
  }

  final case class NameOnly(name: String) extends Argument {
    override def toString = name
    def value = throw new UnsupportedOperationException(s"Option $name has no value")
  }

  final case class NameValue(name: String, value: String) extends Argument {
    def toInt =
      try value.toInt
      catch { case e: NumberFormatException ⇒ throw new IllegalArgumentException(s"Number expected: $toString")}

    override def toString = s"$name$value"
  }

  final case class ValueOnly(value: String) extends Argument {
    def name = ""
  }
}
