package js7.data.node

import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.data.event.SnapshotableState

trait NodeNameToPassword[S <: SnapshotableState[S]]
extends (NodeName => Checked[Option[SecretString]])

object NodeNameToPassword {
}
