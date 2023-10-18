package js7.common.commandline

import java.util.NoSuchElementException
import js7.base.convert.ConvertibleMultiPartialFunction
import js7.common.commandline.CommandLineArguments.*
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class CommandLineArguments private(argMap: mutable.LinkedHashMap[String, Vector[Argument]])
extends PartialFunction[String, Vector[String]]
with ConvertibleMultiPartialFunction[String, String]:

  private val unusedArguments = new mutable.LinkedHashMap[String, Vector[Argument]] ++ argMap
  private val unusedKeylessArguments = new mutable.HashSet[Int]() ++ apply("").indices

  def boolean(key: String, default: Boolean = false): Boolean =
    arguments(key) match
      case Vector() => default
      case Vector(Switch(_, value)) => value
      case Vector(Switch(_, _), Switch(_, value)) => value  // First switch is overriden by second switch
      case _ => throw new IllegalArgumentException(s"Multiple command line options '$key'")

  def keylessValue(index: Int): String =
    unusedKeylessArguments -= index
    val a = arguments("")
    if index >= a.size then throw new NoSuchElementException(s"Too few keyless arguments: argument #${index+1} expected")
    a(index).value

  lazy val keylessValues: Vector[String] =
    unusedKeylessArguments.clear()
    values("")

  def isDefinedAt(key: String) = argMap isDefinedAt key

  def apply(key: String): Vector[String] = values(key)

  private def values(key: String): Vector[String] = arguments(key).map(_.value)

  def arguments(key: String): Vector[Argument] =
    unusedArguments -= key
    argMap(key)

  override protected def renderKey(key: String) = s"command line option '$key'"

  def requireNoMoreArguments(): Unit =
    if unusedArguments.nonEmpty || unusedKeylessArguments.nonEmpty then
      throw new IllegalArgumentException("Unknown command line arguments: " + unusedKeylessArguments.map(o => s"#${o+1}") .mkString(" ") +
        unusedArguments.values.flatten.mkString(" "))

  override def toString = argMap.values.flatten.mkString(" ")


object CommandLineArguments:
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  private val OptionWithValueRegex = "(?s)(-[^=]+=)(.*)".r   // "(?s)" to match multi-line arguments

  def apply(args: Seq[String]): CommandLineArguments =
    val m = new mutable.LinkedHashMap[String, Vector[Argument]]:
      override def default(key: String) = Vector.empty//throw new IllegalArgumentException(if (key.nonEmpty) s"Missing option -$key" else s"Missing argument")
    for a <- args.lastOption if a endsWith "\r" do
      throw new IllegalArgumentException("The last argument must not end with a CR (\\r)")
    for a <- parseArgs(args) do
      m.get(a.key) match
        case None => m += a.key -> Vector(a)
        case Some(seq) => m(a.key) = seq :+ a
    new CommandLineArguments(m)

  private def parseArgs(args: Seq[String]): List[Argument] = (args map toArgument).toList

  private def toArgument(string: String): Argument =
    string match
      case OptionWithValueRegex(key, value) => NameValue(key, value)
      case "-" => ValueOnly("-")
      case o if string.startsWith("-") && string.endsWith("-") => Switch(o dropRight 1, false)
      case o if string.startsWith("-") => Switch(o, true)
      case o => ValueOnly(o)

  def parse[A](args: Seq[String])(convert: CommandLineArguments => A): A =
    val arguments = CommandLineArguments(args)
    val result = convert(arguments)
    arguments.requireNoMoreArguments()
    result

  sealed trait Argument:
    def key: String
    def value: String

  final case class Switch(key: String, booleanValue: Boolean) extends Argument:
    override def toString = key
    def value = throw new UnsupportedOperationException(s"Option $key has no value")

  final case class NameValue(key: String, value: String) extends Argument:
    override def toString = s"$key$value"

  final case class ValueOnly(value: String) extends Argument:
    def key = ""
    override def toString = value
