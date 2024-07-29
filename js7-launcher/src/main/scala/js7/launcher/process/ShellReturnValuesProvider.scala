package js7.launcher.process

import java.nio.charset.Charset
import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileDeleter
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.utils.AutoClosing.autoClosing
import js7.data.value.{NamedValues, StringValue}
import js7.launcher.process.ShellReturnValuesProvider.*

/**
  * @author Joacim Zschimmer
  */
private final class ShellReturnValuesProvider(
  tmpDirectory: Path,
  encoding: Charset,
  v1Compatible: Boolean = false):

  private var fileExists = false

  val file: Path =
    val file = createTempFile(tmpDirectory, "returnValues-", ".tmp")
    fileExists = true
    file

  def clear(): Unit =
    file := ""

  def tryDeleteFile(): Unit =
    if fileExists then
      FileDeleter.tryDeleteFile(file)
      fileExists = false

  def toEnv: (String, String) =
    varName -> file.toString

  def read(): NamedValues =
    autoClosing(scala.io.Source.fromFile(file.toFile)(encoding)): source =>
      source.getLines().map(lineToNamedvalue).toMap

  private def lineToNamedvalue(line: String): (String, StringValue) =
    line match
      case ReturnValuesRegex(name, value) => name.trim -> StringValue(value.trim)
      case _ => throw new IllegalArgumentException(
        "Not the expected syntax NAME=VALUE in files denoted by environment variable " +
          s"$varName: $line")

  def varName: String =
    if v1Compatible then V1VarName else VarName

  override def toString = file.toString

private object ShellReturnValuesProvider:
  private val logger = Logger[this.type]
  private val V1VarName = "SCHEDULER_RETURN_VALUES"
  private val VarName = "JS7_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r
