package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.Event
import com.typesafe.config.Config

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, config: Config): (Recovered[MasterState, Event], ClusterState) = {
    val stateBuilder = new MasterStateBuilder
    val recovered = JournaledStateRecoverer.recover[MasterState, Event](journalMeta, stateBuilder, config)
    (recovered, stateBuilder.clusterState)
  }
}
