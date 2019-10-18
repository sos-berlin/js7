package com.sos.jobscheduler.master

import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.state.{JournaledStateRecoverer, Recovered}
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.Event
import com.sos.jobscheduler.master.configuration.MasterConfiguration

object MasterJournalRecoverer
{
  def recover(journalMeta: JournalMeta, masterConfiguration: MasterConfiguration): (Recovered[MasterState, Event], ClusterState) = {
    var masterStateBuilder: MasterStateBuilder = null
    val recovered = JournaledStateRecoverer.recover[MasterState, Event](
      journalMeta,
      () => {
        // Hack to access clusterState until MasterState becomes a JournaledState
        masterStateBuilder = new MasterStateBuilder(masterConfiguration.clusterConf.nodeId, masterConfiguration.clusterConf.role)
        masterStateBuilder
      },
      masterConfiguration.config)
    (recovered, masterStateBuilder.clusterState)
  }
}
