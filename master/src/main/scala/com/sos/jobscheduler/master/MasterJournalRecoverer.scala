package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.configuration.MasterConfiguration

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, masterConfiguration: MasterConfiguration): (Recovered[MasterState, Event], ClusterState) = {
    val stateBuilder = new MasterStateBuilder(masterConfiguration.clusterConf.nodeId, masterConfiguration.clusterConf.role)
    val recovered = JournaledStateRecoverer.recover[MasterState, Event](journalMeta, stateBuilder, masterConfiguration.config)
    (recovered, stateBuilder.clusterState)
  }
}
