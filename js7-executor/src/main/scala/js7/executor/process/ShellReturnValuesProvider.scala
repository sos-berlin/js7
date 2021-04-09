package js7.executor.process

import java.nio.file.Files.{createTempFile, deleteIfExists}
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax._
import js7.base.utils.AutoClosing.autoClosing
import js7.data.value.{NamedValues, StringValue}
import js7.executor.configuration.JobExecutorConf.FileEncoding
import js7.executor.process.ShellReturnValuesProvider._

/**
  * @author Joacim Zschimmer
  */
final class ShellReturnValuesProvider(temporaryDirectory: Path, v1Compatible: Boolean = false)
{
  private var fileExists = false

  val file: Path = {
    val file = createTempFile(temporaryDirectory, "returnValues-", ".tmp")
    fileExists = true
    file
  }

  def clear(): Unit =
    file := ""

  def deleteFile(): Unit = {
    if (fileExists) {
      deleteIfExists(file)
      fileExists = false
    }
  }

  def toEnv: (String, String) =
    varName -> file.toString

  def read(): NamedValues =
    autoClosing(scala.io.Source.fromFile(file.toFile)(FileEncoding)) { source =>
      (source.getLines() map lineToNamedvalue).toMap
    }

  private def lineToNamedvalue(line: String): (String, StringValue) =
    line match {
      case ReturnValuesRegex(name, value) => name.trim -> StringValue(value.trim)
      case _ => throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in files denoted by environment variable " +
        s"$varName: $line")
    }

  def varName = if (v1Compatible) V1VarName else VarName

  override def toString = file.toString
}

object ShellReturnValuesProvider
{
  private val V1VarName = "SCHEDULER_RETURN_VALUES"
  private val VarName = "JS7_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r
}
