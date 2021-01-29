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
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.value.expression.Expression.ObjectExpression

sealed trait Executable {
  def v1Compatible: Boolean
  def env: ObjectExpression
}

sealed trait PathExecutable
extends Executable
{
  def path: String

  def isAbsolute: Boolean
}
object PathExecutable
{
  def unapply(pathExecutable: PathExecutable) =
    Some((pathExecutable.path, pathExecutable.env, pathExecutable.v1Compatible))

  def apply(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean = false) =
    unchecked(path, env, v1Compatible)

  protected def unchecked(path: String, env: ObjectExpression, v1Compatible: Boolean = false) =
    if (isAbsolute(path))
      AbsolutePathExecutable.checked(path, env, v1Compatible).orThrow
    else
      RelativePathExecutable.checked(path, env, v1Compatible).orThrow

  def sh(path: String, env: ObjectExpression = ObjectExpression.empty) =
    apply(if (isWindows) s"$path.cmd" else path, env)

  def checked(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean = false): Checked[PathExecutable] =
    if (isAbsolute(path))
      AbsolutePathExecutable.checked(path, env, v1Compatible)
    else
      RelativePathExecutable.checked(path, env, v1Compatible)

  private[job] def isAbsolute(path: String) =
    path.startsWith("/") || path.startsWith("\\")/*also on Unix, to be reliable*/

  val jsonEncoder: Encoder.AsObject[PathExecutable] =
    o => JsonObject(
      TypedJsonCodec.typeField[PathExecutable],
      "path" -> o.path.asJson,
      "env" -> o.env.??.asJson,
      "v1Compatible" -> o.v1Compatible.?.asJson)

  val jsonDecoder: Decoder[PathExecutable] =
    cursor => for {
      path <-cursor.get[String]("path")
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
      pathExecutable <- PathExecutable.checked(path, env, v1Compatible).toDecoderResult(cursor.history)
    } yield pathExecutable
}

final case class AbsolutePathExecutable private(
  path: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends PathExecutable
{
  assert(PathExecutable.isAbsolute(path))

  def isAbsolute = true
}
object AbsolutePathExecutable {
  def checked(path: String, env: ObjectExpression = ObjectExpression.empty, v1Compatible: Boolean) =
    if (path.isEmpty)
      Left(EmptyStringProblem(path))
    else if (!PathExecutable.isAbsolute(path))
      Left(InvalidNameProblem("AbsolutePathExecutable", path))
    else
      Right(new AbsolutePathExecutable(path, env, v1Compatible))
}

final case class RelativePathExecutable private(
  path: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends PathExecutable
{
  assert(!PathExecutable.isAbsolute(path))

  def isAbsolute = false

  def toFile(directory: Path): Path =
    directory resolve path.stripPrefix("/")
}
object RelativePathExecutable {
  def checked(
    path: String,
    env: ObjectExpression = ObjectExpression.empty,
    v1Compatible: Boolean)
  : Checked[RelativePathExecutable] =
    if (path.isEmpty)
      Left(EmptyStringProblem("RelativePathExecutable"))
    else if (PathExecutable.isAbsolute(path) || path.contains('\\') || path.startsWith(".")
      || path.contains("/.") || path.head.isWhitespace || path.last.isWhitespace)
      Left(InvalidNameProblem("RelativePathExecutable", path))
    else
      Right(new RelativePathExecutable(path, env, v1Compatible))
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
      "env" -> o.env.??.asJson)

  implicit val jsonDecoder: Decoder[CommandLineExecutable] =
    cursor => for {
      commandLine <- cursor.get[String]("command")
      commandExpr <- CommandLineParser.parse(commandLine).toDecoderResult(cursor.history)
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
    } yield CommandLineExecutable(commandExpr, env)
}

final case class ScriptExecutable(
  script: String,
  env: ObjectExpression = ObjectExpression.empty,
  v1Compatible: Boolean = false)
extends Executable
object ScriptExecutable
{
  implicit val jsonEncoder: Encoder.AsObject[ScriptExecutable] =
    o => JsonObject(
      "script" -> o.script.asJson,
      "env" -> o.env.??.asJson,
      "v1Compatible" -> o.v1Compatible.?.asJson)

  implicit val jsonDecoder: Decoder[ScriptExecutable] =
    cursor => for {
      script <- cursor.get[String]("script")
      env <- cursor.getOrElse[ObjectExpression]("env")(ObjectExpression.empty)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
    } yield ScriptExecutable(script, env, v1Compatible)
}

object Executable
{
  implicit val jsonCodec: TypedJsonCodec[Executable] = TypedJsonCodec(
    Subtype.named(PathExecutable.jsonEncoder, PathExecutable.jsonDecoder,
      Seq(
        classOf[AbsolutePathExecutable],
        classOf[RelativePathExecutable]),
      aliases = Seq("ExecutablePath")),
    Subtype.named[ScriptExecutable](aliases = Seq("ExecutableScript")),
    Subtype[CommandLineExecutable])
}
