package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.core.common.VariablesXmlParser
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import com.sos.jobscheduler.data.job.{JobId, JobPath}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
final case class JobConfiguration(
  id: JobId,
  script: JobScript,
  variables: Map[String, String] = Map(),
  taskLimit: Int = JobConfiguration.DefaultTaskLimit)
extends FileBased
{
  type Self = JobConfiguration

  val companion = JobConfiguration

  def withId(id: FileBasedId[JobPath]) = copy(id = id)

  def language = "shell"
}

object JobConfiguration extends FileBased.Companion[JobConfiguration]
{
  type ThisFileBased = JobConfiguration
  type Path = JobPath

  def typedPathCompanion = JobPath

  private val DefaultTaskLimit = 1

  implicit val jsonEncoder: ObjectEncoder[JobConfiguration] = job ⇒
    JsonObject(
      "id" → (!job.id.isAnonymous ? job.id).asJson,
      "script" → Json.fromString(job.script.string),
      "variables" → job.variables.asJson,
      "taskLimit" → Json.fromInt(job.taskLimit))

  implicit val jsonDecoder: Decoder[JobConfiguration] =
    cursor ⇒
      for {
        id ← cursor.get[Option[JobId]]("id") map (_ getOrElse JobPath.NoId)
        script ← cursor.get[String]("script") map JobScript.apply
        variables ← cursor.get[Map[String, String]]("variables")
        taskLimit ← cursor.get[Int]("taskLimit")
      } yield JobConfiguration(id, script, variables, taskLimit)

  def parseXml(id: JobId, source: Source): Checked[JobConfiguration] =
    Checked.catchNonFatal {
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
            id,
            variables = elements.option[Map[String, String]]("params") getOrElse Map(),
            script = elements.one[JobScript],
            taskLimit = taskLimit)
        }
      }
    }
}
