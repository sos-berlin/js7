package js7.data.job

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.io.process.{KeyLogin, ReturnCode}
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.order.Outcome
import js7.data.value.expression.Expression
import js7.data.value.{NamedValues, NumberValue}

sealed trait Executable
{
  def arguments: Map[String, Expression]

  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    arguments.values.view.flatMap(_.referencedJobResourcePaths)
}

sealed trait ProcessExecutable extends Executable
{
  final def arguments = Map.empty

  override def referencedJobResourcePaths =
    super.referencedJobResourcePaths ++
      env.values.view.flatMap(_.referencedJobResourcePaths)

  def env: Map[String, Expression]

  def returnCodeMeaning: ReturnCodeMeaning

  def login: Option[KeyLogin]

  def v1Compatible: Boolean

  final def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
    Outcome.Completed(
      success = returnCodeMeaning.isSuccess(returnCode),
      namedValues + ("returnCode" -> NumberValue(returnCode.number)))
}

sealed trait PathExecutable
extends ProcessExecutable
{
  def path: String

  def isAbsolute: Boolean
}
object PathExecutable
{
  def unapply(pathExecutable: PathExecutable) =
    Some((pathExecutable.path, pathExecutable.env, pathExecutable.returnCodeMeaning,
      pathExecutable.login, pathExecutable.v1Compatible))

  def apply(
    path: String,
    env: Map[String, Expression] = Map.empty,
    login: Option[KeyLogin] = None,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    v1Compatible: Boolean = false)
  =
    unchecked(path, env, login, returnCodeMeaning, v1Compatible)

  protected def unchecked(
    path: String,
    env: Map[String, Expression],
    login: Option[KeyLogin] = None,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    v1Compatible: Boolean = false)
  =
    if (isAbsolute(path))
      AbsolutePathExecutable.checked(path, env, login, returnCodeMeaning, v1Compatible).orThrow
    else
      RelativePathExecutable.checked(path, env, login, returnCodeMeaning, v1Compatible).orThrow

  def sh(path: String, env: Map[String, Expression] = Map.empty) =
    apply(if (isWindows) s"$path.cmd" else path, env)

  def checked(
    path: String,
    env: Map[String, Expression] = Map.empty,
    login: Option[KeyLogin] = None,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    v1Compatible: Boolean = false)
  : Checked[PathExecutable] =
    if (isAbsolute(path))
      AbsolutePathExecutable.checked(path, env, login, returnCodeMeaning, v1Compatible)
    else
      RelativePathExecutable.checked(path, env, login, returnCodeMeaning, v1Compatible)

  private[job] def isAbsolute(path: String) =
    path.startsWith("/") || path.startsWith("\\")/*also on Unix, to be reliable*/

  val jsonEncoder: Encoder.AsObject[PathExecutable] =
    o => JsonObject(
      TypedJsonCodec.typeField[PathExecutable],
      "path" -> o.path.asJson,
      "env" -> o.env.??.asJson,
      "login" -> o.login.asJson,
      "returnCodeMeaning" -> o.returnCodeMeaning.??.asJson,
      "v1Compatible" -> o.v1Compatible.?.asJson)

  val jsonDecoder: Decoder[PathExecutable] =
    cursor => for {
      path <-cursor.get[String]("path")
      env <- cursor.getOrElse[Map[String, Expression]]("env")(Map.empty)
      login <- cursor.get[Option[KeyLogin]]("login")
      returnCodeMeaning <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
      pathExecutable <- PathExecutable.checked(path, env, login, returnCodeMeaning, v1Compatible).toDecoderResult(cursor.history)
    } yield pathExecutable
}

final case class AbsolutePathExecutable private(
  path: String,
  env: Map[String, Expression] = Map.empty,
  login: Option[KeyLogin] = None,
  returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
  v1Compatible: Boolean = false)
extends PathExecutable
{
  assert(PathExecutable.isAbsolute(path))

  def isAbsolute = true
}
object AbsolutePathExecutable {
  def checked(
    path: String,
    env: Map[String, Expression] = Map.empty,
    login: Option[KeyLogin] = None,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    v1Compatible: Boolean) =
    if (path.isEmpty)
      Left(EmptyStringProblem(path))
    else if (!PathExecutable.isAbsolute(path))
      Left(InvalidNameProblem("AbsolutePathExecutable", path))
    else
      Right(new AbsolutePathExecutable(path, env, login, returnCodeMeaning, v1Compatible))
}

final case class RelativePathExecutable private(
  path: String,
  env: Map[String, Expression] = Map.empty,
  login: Option[KeyLogin] = None,
  returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
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
    env: Map[String, Expression] = Map.empty,
    login: Option[KeyLogin] = None,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    v1Compatible: Boolean = false)
  : Checked[RelativePathExecutable] =
    if (path.isEmpty)
      Left(EmptyStringProblem("RelativePathExecutable"))
    else if (PathExecutable.isAbsolute(path) || path.contains('\\') || path.startsWith(".")
      || path.contains("/.") || path.head.isWhitespace || path.last.isWhitespace)
      Left(InvalidNameProblem("RelativePathExecutable", path))
    else
      Right(new RelativePathExecutable(path, env, login, returnCodeMeaning, v1Compatible))
}

final case class CommandLineExecutable(
  commandLineExpression: CommandLineExpression,
  env: Map[String, Expression] = Map.empty,
  login: Option[KeyLogin] = None,
  returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default)
extends ProcessExecutable {
  def v1Compatible = false
}
object CommandLineExecutable
{
  def fromString(commandLine: String, env: Map[String, Expression] = Map.empty) =
    CommandLineParser.parse(commandLine).map(apply(_, env))

  implicit val jsonEncoder: Encoder.AsObject[CommandLineExecutable] =
    o => JsonObject(
      "command" -> o.commandLineExpression.toString.asJson,
      "env" -> o.env.??.asJson,
      "login" -> o.login.asJson,
      "returnCodeMeaning" -> o.returnCodeMeaning.??.asJson)

  implicit val jsonDecoder: Decoder[CommandLineExecutable] =
    cursor => for {
      commandLine <- cursor.get[String]("command")
      commandExpr <- CommandLineParser.parse(commandLine).toDecoderResult(cursor.history)
      env <- cursor.getOrElse[Map[String, Expression]]("env")(Map.empty)
      login <- cursor.get[Option[KeyLogin]]("login")
      returnCodeMeaning <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
    } yield CommandLineExecutable(commandExpr, env, login, returnCodeMeaning)
}

final case class ShellScriptExecutable(
  script: String,
  env: Map[String, Expression] = Map.empty,
  login: Option[KeyLogin] = None,
  returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
  v1Compatible: Boolean = false)
extends ProcessExecutable
object ShellScriptExecutable
{
  implicit val jsonEncoder: Encoder.AsObject[ShellScriptExecutable] =
    o => JsonObject(
      "script" -> o.script.asJson,
      "env" -> o.env.??.asJson,
      "login" -> o.login.asJson,
      "returnCodeMeaning" -> o.returnCodeMeaning.??.asJson,
      "v1Compatible" -> o.v1Compatible.?.asJson)

  implicit val jsonDecoder: Decoder[ShellScriptExecutable] =
    cursor => for {
      script <- cursor.get[String]("script")
      env <- cursor.getOrElse[Map[String, Expression]]("env")(Map.empty)
      login <- cursor.get[Option[KeyLogin]]("login")
      returnCodeMeaning <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      v1Compatible <- cursor.getOrElse[Boolean]("v1Compatible")(false)
    } yield ShellScriptExecutable(script, env, login, returnCodeMeaning, v1Compatible)
}

final case class InternalExecutable(
  className: String,
  /** Arguments for the job itself. */
  jobArguments: Map[String, Expression] = Map.empty,
  /** Argument expressions evaluated for each `processOrder`. */
  arguments: Map[String, Expression] = Map.empty)
extends Executable

object Executable
{
  private implicit val customConfig = withDefaults

  implicit val jsonCodec = TypedJsonCodec[Executable](
    Subtype.named(PathExecutable.jsonEncoder, PathExecutable.jsonDecoder,
      Seq(
        classOf[AbsolutePathExecutable],
        classOf[RelativePathExecutable]),
      aliases = Seq("ExecutablePath")),
    Subtype.named[ShellScriptExecutable](aliases = Seq("ExecutableScript", "ScriptExecutable")),
    Subtype[CommandLineExecutable],
    Subtype(deriveConfiguredCodec[InternalExecutable]))
}
