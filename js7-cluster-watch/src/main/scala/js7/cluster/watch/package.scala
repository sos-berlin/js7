package js7.cluster

import js7.base.eventbus.StandardEventBus
import js7.data.cluster.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem

package object watch:
  type ClusterWatchEventBus = StandardEventBus[ClusterNodeLossNotConfirmedProblem]
