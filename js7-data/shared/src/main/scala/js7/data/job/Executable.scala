package js7.data.job

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.system.OperatingSystem.isWindows

sealed trait Executable

sealed trait ExecutablePath
extends Executable with GenericString
{
  def path: String
  def string = path

  def isAbsolute: Boolean
}
object ExecutablePath extends GenericString.Checked_[ExecutablePath]
{
  def unapply(executablePath: ExecutablePath) = Some(executablePath.path)

  protected def unchecked(path: String) =
    if (isAbsolute(path))
      AbsoluteExecutablePath.checked(path).orThrow
    else
      RelativeExecutablePath.checked(path).orThrow

  def sh(path: String) = apply(if (isWindows) s"$path.cmd" else path)

  override def checked(path: String): Checked[ExecutablePath] =
    if (isAbsolute(path))
      AbsoluteExecutablePath.checked(path)
    else
      RelativeExecutablePath.checked(path)

  private[job] def isAbsolute(path: String) =
    path.startsWith("/") || path.startsWith("\\")/*also on Unix, to be reliable*/

  override val jsonEncoder: Encoder.AsObject[ExecutablePath] =
    o => JsonObject(
      TypedJsonCodec.TypeFieldName -> "ExecutablePath".asJson,
      "path" -> o.path.asJson)

  override val jsonDecoder: Decoder[ExecutablePath] =
    cursor => cursor.get[String]("path") map ExecutablePath.apply
}

final case class AbsoluteExecutablePath private(path: String)
extends Executable with ExecutablePath
{
  assert(ExecutablePath.isAbsolute(path))

  def isAbsolute = true
}
object AbsoluteExecutablePath {
  def checked(path: String) =
    if (path.isEmpty)
      Left(EmptyStringProblem(path))
    else if (!ExecutablePath.isAbsolute(path))
      Left(InvalidNameProblem("AbsoluteExecutablePath", path))
    else
      Right(new AbsoluteExecutablePath(path))
}

final case class RelativeExecutablePath private(path: String)
extends Executable with ExecutablePath
{
  assert(!ExecutablePath.isAbsolute(path))

  def isAbsolute = false

  def toFile(directory: Path): Path =
    directory resolve path.stripPrefix("/")
}
object RelativeExecutablePath {
  def checked(path: String): Checked[RelativeExecutablePath] =
    if (path.isEmpty)
      Left(EmptyStringProblem("RelativeExecutablePath"))
    else if (ExecutablePath.isAbsolute(path) || path.contains('\\') || path.startsWith(".")
      || path.contains("/.") || path.head.isWhitespace || path.last.isWhitespace)
      Left(InvalidNameProblem("RelativeExecutablePath", path))
    else
      Right(new RelativeExecutablePath(path))
}

final case class CommandLineExecutable(commandLineExpression: CommandLineExpression)
extends Executable
object CommandLineExecutable
{
  def fromString(commandLine: String) =
    CommandLineParser.parse(commandLine) map apply

  implicit val jsonEncoder: Encoder.AsObject[CommandLineExecutable] =
    o => JsonObject("command" -> o.commandLineExpression.toString.asJson)

  implicit val jsonDecoder: Decoder[CommandLineExecutable] =
    cursor =>
    for {
      commandLine <- cursor.get[String]("command")
      commandExpr <- CommandLineParser.parse(commandLine).toDecoderResult(cursor.history)
    } yield CommandLineExecutable(commandExpr)
}

final case class ExecutableScript(script: String)
extends Executable with GenericString
{
  def string = script
}

object ExecutableScript extends GenericString.Companion[ExecutableScript]

object Executable
{
  implicit val jsonCodec: TypedJsonCodec[Executable] = TypedJsonCodec(
    Subtype(ExecutablePath.jsonEncoder, ExecutablePath.jsonDecoder, Seq(classOf[AbsoluteExecutablePath], classOf[RelativeExecutablePath])),
    Subtype(deriveCodec[ExecutableScript]),
    Subtype[CommandLineExecutable])
}
