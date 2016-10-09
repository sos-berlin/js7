package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.generic.IsString

final case class NodeId(string: String) extends IsString

object NodeId extends IsString.Companion[NodeId]
