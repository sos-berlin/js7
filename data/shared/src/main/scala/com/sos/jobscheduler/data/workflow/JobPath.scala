package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.filebased.TypedPath

final case class JobPath(string: String)
extends TypedPath {

  validate()

  def companion = JobPath

  def toXmlFile = withoutStartingSlash + ".job.xml"
}

object JobPath extends TypedPath.Companion[JobPath]
