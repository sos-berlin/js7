package js7.agent.scheduler.job

import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration.FileEncoding
import js7.agent.scheduler.job.ShellReturnValuesProvider._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.scalautil.FileUtils.syntax._
import js7.data.value.{NamedValues, StringValue}

/**
  * @author Joacim Zschimmer
  */
final class ShellReturnValuesProvider(temporaryDirectory: Path)
{
  val file: Path = createTempFile(temporaryDirectory, "returnValues-", ".tmp")

  def clear(): Unit =
    file := ""

  def env: (String, String) =
    ReturnValuesFileEnvironmentVariableName -> file.toString

  def variables: NamedValues =
    autoClosing(scala.io.Source.fromFile(file.toFile)(FileEncoding)) { source =>
      (source.getLines() map lineToNamedvalue).toMap
    }

  override def toString = s"ShellReturnValuesProvider($file)"
}

object ShellReturnValuesProvider
{
  private val ReturnValuesFileEnvironmentVariableName = "SCHEDULER_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r

  private def lineToNamedvalue(line: String): (String, StringValue) =
    line match {
      case ReturnValuesRegex(name, value) => name.trim -> StringValue(value.trim)
      case _ => throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in files denoted by environment variable " +
        s"$ReturnValuesFileEnvironmentVariableName: $line")
    }
}
