package js7.data.job

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.expression.Expression.ObjectExpression

sealed trait Executable {
  def v1Compatible: Boolean
  def env: ObjectExpression
}

sealed trait ExecutablePath
extends Executable
{
  def path: String

  def isAbsolute: Boolean
}
object ExecutablePath
{
  def unapply(executablePath: ExecutablePath) =
    Some((executablePath.path, executablePath.env, executablePath.v1Compatible))

  def apply(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean = false) =
    unchecked(path, env, v1Compatible)

  protected def unchecked(path: String, env: ObjectExpression, v1Compatible: Boolean = false) =
    if (isAbsolute(path))
      AbsoluteExecutablePath.checked(path, env, v1Compatible).orThrow
    else
      RelativeExecutablePath.checked(path, env, v1Compatible).orThrow

  def sh(path: String, env: ObjectExpression = ObjectExpression.empty) =
    apply(if (isWindows) s"$path.cmd" else path, env)

  def checked(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean = false): Checked[ExecutablePath] =
    if (isAbsolute(path))
      AbsoluteExecutablePath.checked(path, env, v1Compatible)
    else
      RelativeExecutablePath.checked(path, env, v1Compatible)

  private[job] def isAbsolute(path: String) =
    path.startsWith("/") || path.startsWith("\\")/*also on Unix, to be reliable*/

  val jsonEncoder: Encoder.AsObject[ExecutablePath] =
    o => JsonObject(
      TypedJsonCodec.TypeFieldName -> "ExecutablePath".asJson,
      "path" -> o.path.asJson,
      "env" -> (o.env.nonEmpty ? o.env).asJson,
      "v1Compatible" -> (o.v1Compatible ? true).asJson)

  val jsonDecoder: Decoder[ExecutablePath] =
    cursor => for {
      path <-cursor.get[String]("path")
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
      executablePath <- ExecutablePath.checked(path, env, v1Compatible).toDecoderResult(cursor.history)
    } yield executablePath
}

final case class AbsoluteExecutablePath private(
  path: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends ExecutablePath
{
  assert(ExecutablePath.isAbsolute(path))

  def isAbsolute = true
}
object AbsoluteExecutablePath {
  def checked(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean) =
    if (path.isEmpty)
      Left(EmptyStringProblem(path))
    else if (!ExecutablePath.isAbsolute(path))
      Left(InvalidNameProblem("AbsoluteExecutablePath", path))
    else
      Right(new AbsoluteExecutablePath(path, env, v1Compatible))
}

final case class RelativeExecutablePath private(
  path: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends ExecutablePath
{
  assert(!ExecutablePath.isAbsolute(path))

  def isAbsolute = false

  def toFile(directory: Path): Path =
    directory resolve path.stripPrefix("/")
}
object RelativeExecutablePath {
  def checked(
    path: String,
    env: ObjectExpression = ObjectExpression.empty,
    v1Compatible: Boolean)
  : Checked[RelativeExecutablePath] =
    if (path.isEmpty)
      Left(EmptyStringProblem("RelativeExecutablePath"))
    else if (ExecutablePath.isAbsolute(path) || path.contains('\\') || path.startsWith(".")
      || path.contains("/.") || path.head.isWhitespace || path.last.isWhitespace)
      Left(InvalidNameProblem("RelativeExecutablePath", path))
    else
      Right(new RelativeExecutablePath(path, env, v1Compatible))
}

final case class CommandLineExecutable(
  commandLineExpression: CommandLineExpression,
  env: ObjectExpression = ObjectExpression.empty)
extends Executable {
  def v1Compatible = false
}
object CommandLineExecutable
{
  def fromString(commandLine: String, env: ObjectExpression = ObjectExpression.empty) =
    CommandLineParser.parse(commandLine).map(apply(_, env))

  implicit val jsonEncoder: Encoder.AsObject[CommandLineExecutable] =
    o => JsonObject(
      "command" -> o.commandLineExpression.toString.asJson,
      "env" -> (o.env.nonEmpty ? o.env).asJson)

  implicit val jsonDecoder: Decoder[CommandLineExecutable] =
    cursor => for {
      commandLine <- cursor.get[String]("command")
      commandExpr <- CommandLineParser.parse(commandLine).toDecoderResult(cursor.history)
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
    } yield CommandLineExecutable(commandExpr, env)
}

final case class ExecutableScript(
  script: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends Executable
object ExecutableScript {
  implicit val jsonEncoder: Encoder.AsObject[ExecutableScript] =
    o => JsonObject(
      "script" -> o.script.asJson,
      "env" -> (o.env.nonEmpty ? o.env).asJson,
      "v1Compatible" -> (o.v1Compatible ? true).asJson)

  implicit val jsonDecoder: Decoder[ExecutableScript] =
    cursor => for {
      script <- cursor.get[String]("script")
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
    } yield ExecutableScript(script, env, v1Compatible)
}

object Executable
{
  implicit val jsonCodec: TypedJsonCodec[Executable] = TypedJsonCodec(
    Subtype(ExecutablePath.jsonEncoder, ExecutablePath.jsonDecoder, Seq(classOf[AbsoluteExecutablePath], classOf[RelativeExecutablePath])),
    Subtype[ExecutableScript],
    Subtype[CommandLineExecutable])
}
