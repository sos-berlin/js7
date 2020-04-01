package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.agent.configuration.AgentConfiguration.FileEncoding
import com.sos.jobscheduler.agent.scheduler.job.ShellReturnValuesProvider._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import java.nio.file.Files.createTempFile
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class ShellReturnValuesProvider(temporaryDirectory: Path)
{
  val file: Path = createTempFile(temporaryDirectory, "returnValues-", ".tmp")

  def clear(): Unit = {
    file := ""
  }

  def env: (String, String) =
    ReturnValuesFileEnvironmentVariableName -> file.toString

  def variables: Map[String, String] =
    autoClosing(scala.io.Source.fromFile(file.toFile)(FileEncoding)) { source =>
      (source.getLines map lineToKeyValue).toMap
    }

  override def toString = s"ShellReturnValuesProvider($file)"
}

object ShellReturnValuesProvider {
  private val ReturnValuesFileEnvironmentVariableName = "SCHEDULER_RETURN_VALUES"
  private val ReturnValuesRegex = "([^=]+)=(.*)".r

  private def lineToKeyValue(line: String): (String, String) = line match {
    case ReturnValuesRegex(name, value) => name.trim -> value.trim
    case _ => throw new IllegalArgumentException(s"Not the expected syntax NAME=VALUE in files denoted by environment variable " +
      s"$ReturnValuesFileEnvironmentVariableName: $line")
  }
}
