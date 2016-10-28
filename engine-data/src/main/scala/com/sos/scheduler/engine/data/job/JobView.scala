package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.common.HasViewCompanion

/**
  * @author Joacim Zschimmer
  */
trait JobView {
  def path: JobPath
}

object JobView extends HasViewCompanion.WithKnownSubtypes[JobView] {

  protected val subtypes: Subtypes = Set(JobOverview, JobDescription)
}
