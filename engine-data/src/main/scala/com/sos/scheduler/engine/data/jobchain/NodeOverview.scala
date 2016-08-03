package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype

trait NodeOverview {
  def nodeKey: NodeKey
}

object NodeOverview {
  implicit val MyJsonFormat = TypedJsonFormat[NodeOverview](
    Subtype[SimpleJobNodeOverview]("SimpleJob"),
    Subtype[SinkNodeOverview]("Sink"),
    Subtype[NestedJobChainNodeOverview]("NestedJobChain"),
    Subtype[EndNodeOverview]("End"))
}
