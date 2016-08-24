package com.sos.scheduler.engine.data.filebased

import java.nio.file.Path
import java.time.Instant

/**
 * @author Joacim Zschimmer
 */
trait FileBasedDetails extends HasPath {

  def overview: FileBasedOverview
  def file: Option[Path]
  def fileModifiedAt: Option[Instant]
  def sourceXml: Option[String]

  final def path = overview.path
}
