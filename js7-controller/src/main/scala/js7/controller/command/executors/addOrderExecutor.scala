package js7.controller.command.executors

import cats.syntax.traverse.*
import com.typesafe.config.Config
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.log.Logger
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders}
import js7.data.controller.ControllerStateExecutor
import js7.data.event.EventCalc

private[command] final class AddOrderExecutor(config: Config)
extends CommandEventConverter[AddOrder]:

  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val logger = Logger[this.type]

  def toEventCalc(cmd: AddOrder) =
    EventCalc: coll =>
      coll:
        coll.aggregate.checkPlanIsOpen(cmd.order.planId).flatMap: _ =>
          ControllerStateExecutor
            .addOrder(coll.aggregate, cmd.order, coll.context.now,
              suppressOrderIdCheckFor = suppressOrderIdCheckFor)
            .map:
              case Left(existing) =>
                logger.debug(s"Discarding duplicate added Order: ${cmd.order}")
                Nil
              case Right(orderAddedEvents) =>
                orderAddedEvents.toKeyedEvents


private[command] final class AddOrdersExecutor(config: Config)
extends CommandEventConverter[AddOrders]:

  private val suppressOrderIdCheckFor =
    config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")

  def toEventCalc(cmd: AddOrders) =
    EventCalc: coll =>
      cmd.orders.traverse: order =>
        coll.aggregate.checkPlanIsOpen(order.planId)
      .flatMap: _ =>
        ControllerStateExecutor
          .addOrders(cmd.orders, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
          .calculate(coll)
