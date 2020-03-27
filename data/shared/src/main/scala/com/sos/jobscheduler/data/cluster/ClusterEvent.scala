package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.{JournalPosition, NoKeyEvent}
import scala.collection.immutable.Seq

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  final case class NodesAppointed(uris: Seq[Uri])
  extends ClusterEvent
  {
    assertThat(uris.size == 2 && uris(0) != uris(1))
  }
  object NodesAppointed {
    def checked(uris: Seq[Uri]): Checked[NodesAppointed] =
      if (uris.size == 2) Right(NodesAppointed(uris))
      else Left(Problem("Exactly two URIs are expected"))
  }

  final case class CouplingPrepared(passiveUri: Uri)
  extends ClusterEvent

  type Coupled = Coupled.type
  case object Coupled extends ClusterEvent

  final case class SwitchedOver(uri: Uri)
  extends ClusterEvent

  final case class FailedOver(failedActiveUri: Uri, activatedUri: Uri, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"$FailedOver($failedActiveUri --> $activatedUri, $failedAt)"
  }

  final case class PassiveLost(uri: Uri)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype.named(deriveCodec[NodesAppointed]  , "Cluster.NodesAppointed"),
    Subtype.named(deriveCodec[CouplingPrepared], "Cluster.CouplingPrepared"),
    Subtype.named(Coupled                      , "Cluster.Coupled"),
    Subtype.named(deriveCodec[SwitchedOver]    , "Cluster.SwitchedOver"),
    Subtype.named(deriveCodec[FailedOver]      , "Cluster.FailedOver"),
    Subtype.named(deriveCodec[PassiveLost]     , "Cluster.PassiveLost"))
}
