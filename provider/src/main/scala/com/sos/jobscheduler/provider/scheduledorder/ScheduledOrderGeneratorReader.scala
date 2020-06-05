package js7.provider.scheduledorder

import akka.util.ByteString
import js7.base.problem.Problem
import js7.common.scalautil.xmls.XmlSources.stringToSource
import js7.core.filebased.FileBasedReader
import js7.data.filebased.{FileBasedId, SourceType}
import io.circe.Json
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorReader(timeZone: ZoneId) extends FileBasedReader
{
  val companion = ScheduledOrderGenerator

  def read(id: FileBasedId[ScheduledOrderGeneratorPath], source: ByteString) = {
    case SourceType.Xml => ScheduledOrderGeneratorXmlParser.parseXml(id, source.utf8String, timeZone)
  }

  def convertFromJson(json: Json) = Left(Problem("ScheduledOrderGenerator does not yet support JSON (it is for development only, anyway)"))
}
