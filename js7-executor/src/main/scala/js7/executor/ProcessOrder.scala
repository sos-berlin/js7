package js7.executor

import js7.data.order.Order
import js7.data.value.NamedValues
import js7.data.workflow.Workflow

final case class ProcessOrder(
  order: Order[Order.Processing],
  workflow: Workflow,
  defaultArguments: NamedValues,
  stdObservers: StdObservers)
