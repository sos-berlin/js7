package com.sos.jobscheduler.data.order

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Payload(variables: Map[String, String]) {

  override def toString = {
    val variableString = variables.keys.toVector.sorted.map(k => s"$k=${variables(k)}").mkString(", ").trim
    s"Payload($variableString)"
  }
}

object Payload {
  val empty = Payload(Map())
}
