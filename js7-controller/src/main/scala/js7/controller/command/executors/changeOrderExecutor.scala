package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ChangeOrder
import js7.data.order.OrderEvent.OrderPriorityChanged
import js7.data.order.OrderId

private[command] def changeOrderExecutor: CommandEventConverter[ChangeOrder] =
  CommandEventConverter.checked[ChangeOrder]: (cmd, controllerState) =>
    for
      order <- controllerState.idToOrder.checked(cmd.orderId)
    yield
      cmd.priority.fold(None): priority =>
        priority != order.priority thenSome:
          cmd.orderId <-: OrderPriorityChanged(priority)
