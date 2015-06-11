package com.sos.scheduler.engine.data.filebased

import java.io.File
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
trait FileBasedDetails extends FileBasedOverview {
  def file: Option[File]
  def fileModificationInstant: Option[Instant]
  def sourceXml: Option[String]
}
