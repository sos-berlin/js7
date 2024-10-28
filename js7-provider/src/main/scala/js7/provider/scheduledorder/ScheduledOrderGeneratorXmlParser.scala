package js7.provider.scheduledorder

import java.time.ZoneId
import javax.xml.transform.Source
import js7.base.convert.As
import js7.base.problem.Checked
import js7.base.utils.Collections.*
import js7.common.scalautil.xmls.ScalaXMLEventReader
import js7.core.common.VariablesXmlParser
import js7.data.folder.FolderPath
import js7.data.item.VersionedItemId
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.provider.scheduledorder.oldruntime.{OldSchedule, OldScheduleXmlParser}

/**
  * @author Joacim Zschimmer
  */
object ScheduledOrderGeneratorXmlParser:

  def parseXml(id: VersionedItemId[ScheduledOrderGeneratorPath], source: Source, timeZone: ZoneId): Checked[ScheduledOrderGenerator] =
    Checked.catchNonFatal:
      ScalaXMLEventReader.parseDocument(source) { eventReader =>
        import eventReader.*
        val folderPath = FolderPath.parentOf(id.path)
        parseElement("order"):
          val workflowPath = attributeMap.as("job_chain")(As(o => folderPath.resolve[WorkflowPath](o)))
          val elements = forEachStartElement:
            case "params" => VariablesXmlParser.parse(eventReader).mapValuesStrict(StringValue.apply)
            case "run_time" => OldScheduleXmlParser.parse(eventReader, timeZone)
          ScheduledOrderGenerator(
            id,
            workflowPath,
            elements.option[NamedValues]("params") getOrElse Map.empty,
            elements.one[OldSchedule])
      }
