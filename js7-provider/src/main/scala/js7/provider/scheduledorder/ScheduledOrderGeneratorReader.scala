package js7.provider.scheduledorder

import akka.util.ByteString
import io.circe.Json
import java.time.ZoneId
import js7.base.problem.Problem
import js7.common.scalautil.xmls.XmlSources.stringToSource
import js7.core.item.InventoryItemReader
import js7.data.item.{ItemId, SourceType}

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorReader(timeZone: ZoneId) extends InventoryItemReader
{
  val companion = ScheduledOrderGenerator

  def read(id: ItemId[ScheduledOrderGeneratorPath], source: ByteString) = {
    case SourceType.Xml => ScheduledOrderGeneratorXmlParser.parseXml(id, source.utf8String, timeZone)
  }

  def convertFromJson(json: Json) = Left(Problem("ScheduledOrderGenerator does not yet support JSON (it is for development only, anyway)"))
}
