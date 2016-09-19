package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}

trait NodeOverview {
  def nodeKey: NodeKey

  final def jobChainPath = nodeKey.jobChainPath

  final def nodeId = nodeKey.nodeId
}

object NodeOverview {
  implicit val MyJsonFormat = TypedJsonFormat[NodeOverview](
    Subtype[SimpleJobNodeOverview]("SimpleJob"),
    Subtype[SinkNodeOverview]("Sink"),
    Subtype[NestedJobChainNodeOverview]("NestedJobChain"),
    Subtype[EndNodeOverview]("End"))
}
