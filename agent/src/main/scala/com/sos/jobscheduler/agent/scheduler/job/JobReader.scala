package com.sos.jobscheduler.agent.scheduler.job

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources.simpleByteStringSource
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobId

/**
  * @author Joacim Zschimmer
  */
object JobReader extends FileBasedReader {
  val companion = JobConfiguration

  def read(jobId: JobId, source: ByteString) = {
    case t: SourceType.JsonLike ⇒
      readAnonymousJsonLike[JobConfiguration](t, source) map (_ withId jobId)

    case SourceType.Xml ⇒
      JobConfiguration.parseXml(jobId, simpleByteStringSource(source))
  }
}
