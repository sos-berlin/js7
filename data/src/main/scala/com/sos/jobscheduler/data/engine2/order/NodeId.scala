package com.sos.jobscheduler.data.engine2.order

import com.sos.jobscheduler.base.generic.IsString

final case class NodeId(string: String) extends IsString

object NodeId extends IsString.Companion[NodeId]
