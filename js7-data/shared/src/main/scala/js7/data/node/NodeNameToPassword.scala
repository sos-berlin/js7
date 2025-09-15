package js7.data.node

import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.data.event.SnapshotableState

type NodeNameToPassword[S <: SnapshotableState[S]] =
  NodeName => Checked[Option[SecretString]]
