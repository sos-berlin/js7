package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.core.common.VariablesXmlParser
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.oldruntime.{OldSchedule, OldScheduleXmlParser}
import java.time.ZoneId
import javax.xml.transform.Source

/**
  * @author Joacim Zschimmer
  */
object ScheduledOrderGeneratorXmlParser {

  def parseXml(path: ScheduledOrderGeneratorPath, source: Source, timeZone: ZoneId): Checked[ScheduledOrderGenerator] =
    Checked.catchNonFatal {
      ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
        import eventReader._
        val folderPath = FolderPath parentOf path
        parseElement("order") {
          val workflowPath = attributeMap.as("job_chain")(As(o ⇒ folderPath.resolve[WorkflowPath](o)))
          val elements = forEachStartElement {
            case "params" ⇒ VariablesXmlParser.parse(eventReader)
            case "run_time" ⇒ OldScheduleXmlParser.parse(eventReader, timeZone)
          }
          ScheduledOrderGenerator(
            path,
            workflowPath,
            elements.option[Map[String, String]]("params") getOrElse Map(),
            elements.one[OldSchedule])
        }
      }
    }
}
