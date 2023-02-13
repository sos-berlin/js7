package js7.cluster

import js7.base.eventbus.StandardEventBus
import js7.cluster.watch.api.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem

package object watch {
  type ClusterWatchEventBus = StandardEventBus[ClusterNodeLossNotConfirmedProblem]
}
