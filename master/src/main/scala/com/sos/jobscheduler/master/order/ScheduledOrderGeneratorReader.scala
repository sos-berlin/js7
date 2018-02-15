package com.sos.jobscheduler.master.order

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.stringToSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.SourceType
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorReader(timeZone: ZoneId) extends FileBasedReader
{
  val fileBasedCompanion = ScheduledOrderGenerator

  def read(path: ScheduledOrderGeneratorPath, source: ByteString) = {
    case SourceType.Xml ⇒ ScheduledOrderGeneratorXmlParser.parseXml(path, source.utf8String, timeZone)
  }
}
