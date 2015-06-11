package com.sos.scheduler.engine.data.xmlcommands

import ModifyOrderCommand._
import com.sos.scheduler.engine.data.order.OrderKey

//import org.joda.time.DateTimeZone.UTC
//import org.joda.time.ReadableInstant
//import org.joda.time.format.DateTimeFormat

final case class ModifyOrderCommand(
    orderKey: OrderKey,
    action: Option[Action.Value] = None,
    at: Option[At] = None,
    title: Option[String] = None,
    suspended: Option[Boolean] = None)
extends XmlCommand {

  def xmlElem = <modify_order
    job_chain={orderKey.jobChainPath.string}
    order={orderKey.id.string}
    action={(action map { _.toString }).orNull}
    at={(at map { _.string }).orNull}
    title={title.orNull}
    suspended={(suspended map { _.toString}).orNull}
    />
}


object ModifyOrderCommand {
//  private val yyyymmddhhmmssFormatter = DateTimeFormat forPattern "yyyy-MM-dd HH:mm:ss'Z'"

  def startNow(o: OrderKey) =
    new ModifyOrderCommand(o, at = Some(ModifyOrderCommand.NowAt))

  object Action extends Enumeration {
    val reset = Value
  }

  sealed trait At {
    def string: String
  }

  case object NowAt extends At {
    def string = "now"
  }

}
