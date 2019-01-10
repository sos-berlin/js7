package com.sos.jobscheduler.master.scheduledorder

import akka.util.ByteString
import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.stringToSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.{FileBasedId, SourceType}
import io.circe.Json
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorReader(timeZone: ZoneId) extends FileBasedReader
{
  val companion = ScheduledOrderGenerator

  def read(id: FileBasedId[ScheduledOrderGeneratorPath], source: ByteString) = {
    case SourceType.Xml ⇒ ScheduledOrderGeneratorXmlParser.parseXml(id, source.utf8String, timeZone)
  }

  def convertFromJson(json: Json) = Invalid(Problem("ScheduledOrderGenerator does not yet support JSON (it is for development only, anyway)"))
}
