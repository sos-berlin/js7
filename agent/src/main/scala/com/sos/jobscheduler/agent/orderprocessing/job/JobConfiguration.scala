package com.sos.jobscheduler.agent.orderprocessing.job

import com.sos.jobscheduler.agent.orderprocessing.job.JobConfiguration._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.xmls.{FileSource, ScalaXMLEventReader}
import com.sos.jobscheduler.data.engine2.order.JobPath
import com.sos.jobscheduler.shared.common.VariablesXmlParser
import java.nio.file.Path
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
final case class JobConfiguration(
  path: JobPath,
  variables: Map[String, String] = Map(),
  script: JobScript,
  taskLimit: Int = DefaultTaskLimit)
{
  def language = "shell"
}

object JobConfiguration {

  private val DefaultTaskLimit = 1

  def parseXml(jobPath: JobPath, file: Path): JobConfiguration =
    autoClosing(new FileSource(file)) { src ⇒
      JobConfiguration.parseXml(jobPath, src)
    }

  def parseXml(jobPath: JobPath, source: Source): JobConfiguration =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._

      def parseScript(): JobScript =
        parseElement("script") {
          val languageOption = attributeMap.get("language")
          require(languageOption contains "shell")
          JobScript(eatText())
        }

      parseElement("job") {
        val taskLimit = attributeMap.as[Int]("tasks", DefaultTaskLimit)
        val elements =
          forEachStartElement {
            case "extensions" ⇒ ignoreElement()
            case "settings" ⇒ ignoreElement()
            case "description" ⇒ ignoreElement()
            //case "lock.use" ⇒
            case "params" ⇒ VariablesXmlParser.parse(eventReader)
            //case "environment" ⇒
            //case "login" ⇒
            case "script" ⇒ parseScript()
            //case "monitor" ⇒
            //case "start_when_directory_changed" ⇒
            //case "delay_after_error" ⇒
          }
        JobConfiguration(
          jobPath,
          variables = elements.option[Map[String, String]]("params") getOrElse Map(),
          script = elements.one[JobScript],
          taskLimit = taskLimit)
      }
    }
}
