package js7.provider.scheduledorder

import io.circe.Json
import java.time.ZoneId
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.common.scalautil.xmls.XmlSources.stringToSource
import js7.core.item.VersionedItemReader
import js7.data.item.{SourceType, VersionedItemId}

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorReader(timeZone: ZoneId) extends VersionedItemReader:
  val companion: ScheduledOrderGenerator.type = ScheduledOrderGenerator

  def read(id: VersionedItemId[ScheduledOrderGeneratorPath], source: ByteArray)
  : PartialFunction[SourceType, Checked[ScheduledOrderGenerator]] =
    case SourceType.Xml =>
      ScheduledOrderGeneratorXmlParser.parseXml(id, source.utf8String, timeZone)

  def convertFromJson(json: Json): Checked[ScheduledOrderGenerator] =
    Left(Problem(
      "ScheduledOrderGenerator does not yet support JSON (it is for development only, anyway)"))
