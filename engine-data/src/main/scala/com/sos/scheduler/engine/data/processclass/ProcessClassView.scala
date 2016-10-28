package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.common.HasViewCompanion

/**
  * @author Joacim Zschimmer
  */
trait ProcessClassView

object ProcessClassView extends HasViewCompanion.WithKnownSubtypes[ProcessClassView] {
  protected val subtypes: Subtypes = Set(ProcessClassOverview, ProcessClassDetailed)
}
