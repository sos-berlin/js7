package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceCodec
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.data.delegate.DelegateOverview
import js7.data.system.JavaInformation

final case class SubagentOverview(
  version: String,
  buildId: String,
  startedAt: Timestamp,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)
extends DelegateOverview

object SubagentOverview {
  implicit val jsonCodec: CirceCodec[SubagentOverview] = deriveCodec[SubagentOverview]
}
