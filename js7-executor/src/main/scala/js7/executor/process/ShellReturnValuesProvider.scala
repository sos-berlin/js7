package js7.executor.process

import java.nio.file.Files.{createTempFile, deleteIfExists}
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.data.value.{NamedValues, StringValue}
import js7.executor.configuration.JobExecutorConf.FileEncoding
import js7.executor.process.ShellReturnValuesProvider._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private final class ShellReturnValuesProvider(workDirectory: Path, v1Compatible: Boolean = false)
{
  private var fileExists = false

  val file: Path = {
    val file = createTempFile(workDirectory, "returnValues-", ".tmp")
    fileExists = true
    file
  }

  def clear(): Unit =
    file := ""

  def tryDeleteFile(): Unit =
    if (fileExists) {
      try deleteIfExists(file)
      catch { case NonFatal(t) =>
        logger.error(s"Cannot delete file '$file': ${t.toStringWithCauses}")
        // TODO When Windows locks the file, try delete it later, asynchronously
      }
      fileExists = false
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

private object ShellReturnValuesProvider
{
  private val logger = Logger[this.type]
  private val V1VarName = "SCHEDULER_RETURN_VALUES"
  private val VarName = "JS7_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r
}
