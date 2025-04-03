package js7.controller.command.executors

import cats.syntax.traverse.*
import com.typesafe.config.Config
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.log.Logger
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders}
import js7.data.controller.ControllerStateExecutor
import js7.data.event.EventCalc
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] final class AddOrderExecutor(config: Config)
extends CommandEventConverter[AddOrder]:

  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val logger = Logger[this.type]

  def toEventCalc(cmd: AddOrder) =
    EventCalc: coll =>
      coll.addChecked:
        coll.aggregate.checkPlanIsOpen(cmd.order.planId).flatMap: _ =>
          val instrService = InstructionExecutorService(coll.context.clock)
          ControllerStateExecutor(coll.aggregate)(using instrService)
            .addOrder(cmd.order, suppressOrderIdCheckFor = suppressOrderIdCheckFor).map:
              case Left(existing) =>
                logger.debug(s"Discarding duplicate added Order: ${cmd.order}")
                Nil
              case Right(orderAddedEvents) =>
                orderAddedEvents.toKeyedEvents


private[command] final class AddOrdersExecutor(config: Config)
extends CommandEventConverter[AddOrders]:

  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")

  def toEventCalc(cmd: AddOrders) =
    EventCalc: coll =>
      coll.addChecked:
        cmd.orders.traverse: order =>
          coll.aggregate.checkPlanIsOpen(order.planId)
        .flatMap: _ =>
          val instrService = InstructionExecutorService(coll.context.clock)
          ControllerStateExecutor(coll.aggregate)(using instrService)
            .addOrders(cmd.orders, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
