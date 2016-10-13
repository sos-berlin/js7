package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.order.{OrderKey, OrderState}
import com.sos.scheduler.engine.data.xmlcommands.ModifyOrderCommand._

final case class ModifyOrderCommand(
    orderKey: OrderKey,
    action: Option[Action.Value] = None,
    at: Option[At] = None,
    title: Option[String] = None,
    suspended: Option[Boolean] = None,
    state: Option[OrderState] = None,
    endState: Option[OrderState] = None)
extends XmlCommand {

  def xmlElem = <modify_order
    job_chain={orderKey.jobChainPath.string}
    order={orderKey.id.string}
    action={(action map { _.toString }).orNull}
    at={(at map { _.string }).orNull}
    title={title.orNull}
    suspended={(suspended map { _.toString}).orNull}
    state={(state map { _.string }).orNull}
    end_state={(endState map { _.string }).orNull}
    />
}


object ModifyOrderCommand {
//  private val yyyymmddhhmmssFormatter = DateTimeFormat forPattern "yyyy-MM-dd HH:mm:ss'Z'"

  def startNow(o: OrderKey) = new ModifyOrderCommand(o, at = Some(ModifyOrderCommand.NowAt))

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
