package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.data.base.IsString

final case class ClusterMemberId(string: String) extends IsString


object ClusterMemberId extends IsString.HasJsonFormat[ClusterMemberId]
