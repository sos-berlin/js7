package js7.controller.command.executors

import com.typesafe.config.Config
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.log.Logger
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.{AddOrder, AddOrders}
import js7.data.controller.ControllerStateExecutor
import js7.data.event.EventCalc
import js7.data.execution.workflow.instructions.InstructionExecutorService

private[command] def addOrderExecutor(config: Config) = ToEventCalc[AddOrder]: cmd =>
  val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  EventCalc: coll =>
    coll.addChecked:
      val instrService = InstructionExecutorService(coll.context.clock)
      ControllerStateExecutor(coll.aggregate)(using instrService)
        .addOrder(cmd.order, suppressOrderIdCheckFor = suppressOrderIdCheckFor).map:
          case Left(existing) =>
            val logger = Logger("js7.controller.command.executors.addOrderExecutor")
            logger.debug(s"Discarding duplicate added Order: ${cmd.order}")
            Nil
          case Right(orderAddedEvents) =>
            orderAddedEvents.toKeyedEvents


private[command] def addOrdersExecutor(config: Config) = ToEventCalc[AddOrders]: cmd =>
  val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  EventCalc: coll =>
    coll.addChecked:
      val instrService = InstructionExecutorService(coll.context.clock)
      ControllerStateExecutor(coll.aggregate)(using instrService)
        .addOrders(cmd.orders, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
