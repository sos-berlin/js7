package com.sos.jobscheduler.master.gui

import io.circe.Decoder
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto._

/**
  * @author Joacim Zschimmer
  */
package object data {
  type OrderId = String
  val OrderId = (o: String) ⇒ o
  type NodeId = String
  val NodeId = (o: String) ⇒ o
  type JobnetPath = String
  val JobnetPath = (o: String) ⇒ o
  type AgentPath = String
  val AgentPath = (o: String) ⇒ o

  @JsonCodec
  final case class NodeKey(jobnetPath: JobnetPath, nodeId: NodeId)

  final case class OrderOverview(id: String, nodeKey: NodeKey, state: Order.State)
  object OrderOverview {
    implicit val jsonDecoder: Decoder[OrderOverview] = deriveDecoder[OrderOverview]
  }
}
