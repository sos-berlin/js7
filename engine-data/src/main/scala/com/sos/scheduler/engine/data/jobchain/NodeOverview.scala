package com.sos.scheduler.engine.data.jobchain

import spray.json._

trait NodeOverview {
  def nodeKey: NodeKey
}

object NodeOverview {
  private val TypeName = "$TYPE"
  private val SimpleJobTypeName = JsString("SimpleJob")
  private val SinkTypeName = JsString("Sink")
  private val NestedJobChainTypeName = JsString("NestedJobChain")
  private val EndTypeName = JsString("End")

  implicit object MyJsonFormat extends RootJsonFormat[NodeOverview]{
    def write(o: NodeOverview) =
      o match {
        case o: SimpleJobNodeOverview      ⇒ typed(o.toJson, SimpleJobTypeName)
        case o: SinkNodeOverview           ⇒ typed(o.toJson, SinkTypeName)
        case o: NestedJobChainNodeOverview ⇒ typed(o.toJson, NestedJobChainTypeName)
        case o: EndNodeOverview            ⇒ typed(o.toJson, EndTypeName)
      }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      val untyped = JsObject(fields - TypeName)
      fields(TypeName) match {
        case SimpleJobTypeName      ⇒ untyped.convertTo[SimpleJobNodeOverview]
        case SinkTypeName           ⇒ untyped.convertTo[SinkNodeOverview]
        case NestedJobChainTypeName ⇒ untyped.convertTo[NestedJobChainNodeOverview]
        case EndTypeName            ⇒ untyped.convertTo[EndNodeOverview]
        case typ ⇒ throw new IllegalArgumentException(s"Unknown $$TYPE='$typ' for NodeOverview")
      }
    }
  }

  private def typed(jsValue: JsValue, typ: JsString): JsObject = {
    val o = jsValue.asJsObject
    o.copy(fields = o.fields + (TypeName → typ))
  }
}
