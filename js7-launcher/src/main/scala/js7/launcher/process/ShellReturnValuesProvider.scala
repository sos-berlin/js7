package js7.launcher.process

import cats.effect.{IO, Resource}
import java.nio.charset.Charset
import java.nio.file.Path
import js7.base.io.file.FileDeleter
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.metering.CallMeter
import js7.base.utils.AutoClosing.autoClosing
import js7.data.value.{NamedValues, StringValue}
import js7.launcher.process.ShellReturnValuesProvider.*

/**
  * @author Joacim Zschimmer
  */
private final class ShellReturnValuesProvider private(
  val file: Path,
  encoding: Charset,
  v1Compatible: Boolean = false)
extends AutoCloseable:

  def close(): Unit =
    FileDeleter.tryDeleteFile(file)

  def toEnv: (String, String) =
    varName -> file.toString

  def read: IO[NamedValues] =
    meterReadShellReturnValues:
      IO.interruptible:
        autoClosing(scala.io.Source.fromFile(file.toFile)(using encoding)): source =>
          source.getLines().map(lineToNamedvalue).toMap

  private def lineToNamedvalue(line: String): (String, StringValue) =
    line match
      case ReturnValuesRegex(name, value) => name.trim -> StringValue(value.trim)
      case _ => throw IllegalArgumentException:
        "Not the expected syntax NAME=VALUE in files denoted by environment variable " +
          s"$varName: $line"

  def varName: String =
    if v1Compatible then V1VarName else VarName

  override def toString =
    file.toString


private object ShellReturnValuesProvider:

  private val V1VarName = "SCHEDULER_RETURN_VALUES"
  private val VarName = "JS7_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r
  private val meterReadShellReturnValues = CallMeter("ShellReturnValues")

  def resource(tmpDirectory: Path,
    encoding: Charset,
    v1Compatible: Boolean = false)
  : Resource[IO, ShellReturnValuesProvider] =
    for
      file <- temporaryFileResource(tmpDirectory, prefix = "returnValues-", suffix = ".tmp")
      provider <- Resource.fromAutoCloseable:
        IO:
          ShellReturnValuesProvider(file, encoding, v1Compatible)
    yield
      provider
