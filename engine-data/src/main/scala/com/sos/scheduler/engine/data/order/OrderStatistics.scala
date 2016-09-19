package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.order.OrderStatistics._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderStatistics(
  total: Int,
  notPlanned: Int,
  planned: Int,
  pending: Int,
  running: Int,
  inTask: Int,
  inProcess: Int,
  setback: Int,
  suspended: Int,
  blacklisted: Int,
  permanent: Int,
  fileOrder: Int)
{
  override def toString = List(
      format("total", total),
      format("notPlanned", notPlanned),
      format("planned", planned),
      format("pending", pending),
      format("running", running),
      format("inTask", inTask),
      format("inProcess", inProcess),
      format("setback", setback),
      format("suspended", suspended),
      format("blacklisted", blacklisted),
      format("permanent", permanent),
      format("fileOrder", fileOrder))
    .mkString("OrderStatistics(", " ", ")")
}

object OrderStatistics {
  implicit val MyJsonFormat = jsonFormat12(apply)

  private def format(name: String, number: Int) = s"$name=$number"
}
