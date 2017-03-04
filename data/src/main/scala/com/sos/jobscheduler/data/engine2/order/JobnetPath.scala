package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.data.filebased.TypedPath

final case class JobnetPath(string: String)
extends TypedPath {

  validate()

  def companion = JobnetPath
}


object JobnetPath extends TypedPath.Companion[JobnetPath] {

  override lazy val xmlFilenameExtension = s".job_chain.xml"
}
