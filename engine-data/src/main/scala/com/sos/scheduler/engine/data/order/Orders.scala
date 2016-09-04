package com.sos.scheduler.engine.data.order

import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * Only to package Seq[SomeOrders] into JSON object containing the field `orders`.
  *
  * @author Joacim Zschimmer
  */
final case class Orders[+A](orders: immutable.Seq[A])

object Orders {
  implicit def ordersJsonFormat[A: JsonFormat] = jsonFormat1 { a: immutable.Seq[A] ⇒ Orders(a) }
}
