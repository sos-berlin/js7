package js7.tests.controller.proxy.history

import java.time.Instant
import java.util.Optional
import js7.base.web.Uri
import js7.data.event.{Event, KeyedEvent}
import js7.data.job.ReturnCode
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.proxy.javaapi.JEventAndControllerState
import js7.proxy.javaapi.data.order.JOrderEvent
import js7.proxy.javaapi.data.order.JOrderEvent.{JOrderAdded, JOrderCancelled, JOrderFailed, JOrderFinished, JOrderForked, JOrderJoined, JOrderProcessed, JOrderProcessingStarted, JOrderStdWritten}
import js7.proxy.javaapi.data.{JOrder, JWorkflowPosition}
import js7.proxy.javaapi.utils.VavrUtils.getOrThrow
import js7.tests.controller.proxy.history.InMemoryHistory.{terminatedAt, _}
import js7.tests.controller.proxy.history.JControllerProxyHistoryTester.{TestOrderId, TestWorkflowId}
import js7.tests.testenv.DirectoryProvider.StdoutOutput
import scala.collection.mutable
import scala.jdk.CollectionConverters._

private[history] final class InMemoryHistory
{
  private val _idToOrderEntry = mutable.LinkedHashMap[OrderId, OrderEntry]()

  def idToOrderEntry: java.util.Map[OrderId, OrderEntry] =
    _idToOrderEntry.toMap.asJava

  def handleEventAndState(eventAndState: JEventAndControllerState[Event]): Unit = {
    scribe.info(s"### ${eventAndState.stampedEvent.value} ${eventAndState.state.idToOrder(OrderId("ORDER-1"))}")
    eventAndState.stampedEvent.value match {
      case KeyedEvent(_: OrderId, _: OrderEvent) =>
        handleOrderEvent(eventAndState.asInstanceOf[JEventAndControllerState[OrderEvent]])
      case _ =>
    }
  }

  private def handleOrderEvent(eventAndState: JEventAndControllerState[OrderEvent]): Unit = {
    val timestamp = eventAndState.stampedEvent.timestamp.toInstant
    val orderId = eventAndState.stampedEvent.value.key
    JOrderEvent.of(eventAndState.stampedEvent.value.event) match {
      case event: JOrderAdded =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        _idToOrderEntry.get(orderId) match {
          case None =>
            _idToOrderEntry(orderId) = OrderEntry(
              orderId, Optional.empty, event.arguments, OrderEntry.Cause.Added,
              Optional.of(order.workflowPosition), scheduledFor = event.scheduledFor)

          case Some(existing) =>
            _idToOrderEntry(orderId) = existing.copy(
              parent = Optional.empty,
              startWorkflowPosition = Optional.of(order.workflowPosition),
              scheduledFor = event.scheduledFor,
              terminatedAt = Optional.empty,
              endWorkflowPosition = Optional.empty)
        }

      case event: JOrderForked =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        for (child <- event.children.asScala) {
          _idToOrderEntry.get(child.orderId) match {
            case None =>
              _idToOrderEntry(child.orderId) = OrderEntry(child.orderId, Optional.of(orderId), order.arguments,
                OrderEntry.Cause.Forked, Optional.of(order.workflowPosition), Optional.empty)

            case Some(existing) =>
              _idToOrderEntry(child.orderId) = existing.copy(
                parent = Optional.of(orderId),
                startWorkflowPosition = Optional.of(order.workflowPosition),
                scheduledFor = Optional.empty,
                terminatedAt = Optional.empty,
                endWorkflowPosition = Optional.empty)
          }
        }

      case _: JOrderJoined =>
        val order = getOrThrow(eventAndState.previousState.idToCheckedOrder(orderId))
        val orderState = getOrThrow(order.checkedState(JOrder.forked))
        for (id <- orderState.childOrderIds.asScala) {
          _idToOrderEntry(id) = _idToOrderEntry(id).copy(terminatedAt = Optional.of(terminatedAt/*timestamp*/))
        }

      case JOrderFinished.singleton =>
        val order = getOrThrow(eventAndState.previousState.idToCheckedOrder(orderId))
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case JOrderCancelled.singleton =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case _: JOrderFailed =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case JOrderProcessingStarted.singleton =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        var entry = _idToOrderEntry(orderId)
        if (!entry.startedAt.isPresent) {
          entry = entry.copy(
            startWorkflowPosition = Optional.of(order.workflowPosition),
            startedAt = Optional.of(startedAt/*timestamp*/))
        }
        val agentPath = getOrThrow(order.attached)
        val agentUri = getOrThrow(eventAndState.state.pathToAgentRef(agentPath)).uri
        val maybeJobName = eventAndState.state.idToWorkflow(order.workflowId)
          .flatMap(_.checkedJobName(order.workflowPosition.position))
          .fold(_ => Optional.empty[String], (jobName: WorkflowJob.Name) => Optional.of(jobName.string))
        _idToOrderEntry(orderId) = entry.copy(
          steps =
            (entry.steps.asScala :+
              OrderStepEntry(orderId, order.workflowPosition, agentUri, maybeJobName, order.arguments, startedAt/*timestamp*/)
            ).asJava)

      case event: JOrderProcessed =>
        val order = getOrThrow(eventAndState.state.idToCheckedOrder(orderId))
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).updateLastStep(terminatedAt/*timestamp*/, event.outcome, order.arguments)

      case event: JOrderStdWritten =>
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).addToLog(event.stdoutOrStderr, event.chunk)

      case _ =>
    }
  }

  def orderEntries: Seq[OrderEntry] = _idToOrderEntry.values.toVector
}

private[history] object InMemoryHistory
{
  private val workflowId = TestWorkflowId.underlying
  private val startedAt = Instant.ofEpochMilli(1)
  private val terminatedAt = Instant.ofEpochMilli(2)

  def expectedIdToOrderEntry(agentUris: java.util.List[Uri]): java.util.Map[OrderId, OrderEntry] =
    expectedOrderEntries(agentUris.asScala.toVector).map(o => o.orderId -> o).toMap.asJava

  private def expectedOrderEntries(agentUris: IndexedSeq[Uri]) = {
    val keyValues = Map("KEY" -> "VALUE").asJava
    Vector(
      OrderEntry(
        TestOrderId,
        Optional.empty,
        keyValues,
        OrderEntry.Cause.Added,
        Optional.of(JWorkflowPosition(workflowId /: Position(0))),
        Optional.empty,
        Optional.of(startedAt),
        Optional.of(terminatedAt),
        Optional.of(JWorkflowPosition(workflowId /: Position(3))),
        Vector(
          OrderStepEntry(TestOrderId,
            JWorkflowPosition(workflowId /: Position(0)),
            agentUri = agentUris(0),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER"),
            JWorkflowPosition(workflowId /: Position(2)),
            agentUris(0),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava),
      OrderEntry(OrderId("ORDER/🥕"),
        Optional.of(OrderId("ORDER")),
        keyValues,
        OrderEntry.Cause.Forked,
        Optional.of(JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 0))),
        Optional.empty,
        Optional.of(startedAt),
        Optional.of(terminatedAt),
        Optional.empty,
        steps = Vector(
          OrderStepEntry(OrderId("ORDER/🥕"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 0)),
            agentUris(0),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER/🥕"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 1)),
            agentUris(0),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava),
      OrderEntry(OrderId("ORDER/🍋"),
        Optional.of(OrderId("ORDER")),
        keyValues,
        OrderEntry.Cause.Forked,
        Optional.of(JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 0))),
        Optional.empty,
        Optional.of(startedAt),
        Optional.of(terminatedAt),
        Optional.empty,
        steps = Vector(
          OrderStepEntry(OrderId("ORDER/🍋"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 0)),
            agentUris(0),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER/🍋"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 1)),
            agentUris(1),
            jobName = Optional.empty,
            keyValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(ReturnCode(0)),
            Optional.of(keyValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava))
    }
}
