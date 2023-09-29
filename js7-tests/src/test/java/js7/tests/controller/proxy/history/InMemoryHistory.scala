package js7.tests.controller.proxy.history
import java.time.Instant
import java.util.Optional
import js7.base.time.JavaTimestamp.specific.*
import js7.base.web.Uri
import js7.data.event.{Event, EventId, KeyedEvent}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.value.{NamedValues, StringValue, Value}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data_for_java.order.JOrderEvent.{JOrderAdded, JOrderCancelled, JOrderFailed, JOrderFinished, JOrderForked, JOrderJoined, JOrderProcessed, JOrderProcessingStarted, JOrderStdWritten}
import js7.data_for_java.order.{JOrder, JOrderEvent}
import js7.data_for_java.vavr.VavrUtils.getOrThrow
import js7.data_for_java.workflow.position.JWorkflowPosition
import js7.proxy.javaapi.data.controller.JEventAndControllerState
import js7.tests.controller.proxy.history.InMemoryHistory.*
import js7.tests.controller.proxy.history.JControllerApiHistoryTester.{TestOrderId, TestWorkflowId}
import js7.tests.testenv.DirectoryProvider.StdoutOutput
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

private[history] final class InMemoryHistory
{
  private val _idToOrderEntry = mutable.LinkedHashMap[OrderId, OrderEntry]()
  private var _eventId = EventId.BeforeFirst

  def idToOrderEntry: java.util.Map[OrderId, OrderEntry] =
    _idToOrderEntry.toMap.asJava

  def eventId = _eventId

  def update(eventAndState: JEventAndControllerState[Event]): Unit = {
    eventAndState.stampedEvent.value match {
      case KeyedEvent(_: OrderId, _: OrderEvent) =>
        handleOrderEvent(eventAndState.asInstanceOf[JEventAndControllerState[OrderEvent]])
      case _ =>
    }
    _eventId = eventAndState.stampedEvent.eventId
  }

  private def handleOrderEvent(eventAndState: JEventAndControllerState[OrderEvent]): Unit = {
    val timestamp = eventAndState.stampedEvent.timestamp.toInstant
    val orderId = eventAndState.stampedEvent.value.key
    JOrderEvent.of(eventAndState.stampedEvent.value.event) match {
      case event: JOrderAdded =>
        val order = eventAndState.state.idToOrder.get(orderId)
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
        val order = eventAndState.state.idToOrder.get(orderId)
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
        val order = eventAndState.previousState.idToOrder.get(orderId)
        val orderState = getOrThrow(order.checkedState(JOrder.forked))
        for (id <- orderState.childOrderIds.asScala) {
          _idToOrderEntry(id) = _idToOrderEntry(id).copy(terminatedAt = Optional.of(terminatedAt/*timestamp*/))
        }

      case JOrderFinished.singleton =>
        val order = eventAndState.previousState.idToOrder.get(orderId)
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case JOrderCancelled.singleton =>
        val order = eventAndState.state.idToOrder.get(orderId)
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case _: JOrderFailed =>
        val order = eventAndState.state.idToOrder.get(orderId)
        _idToOrderEntry(orderId) = _idToOrderEntry(orderId).copy(
          terminatedAt = Optional.of(terminatedAt/*timestamp*/),
          endWorkflowPosition = Optional.of(order.workflowPosition))

      case _: JOrderProcessingStarted =>
        val order = eventAndState.state.idToOrder.get(orderId)
        var entry = _idToOrderEntry(orderId)
        if (!entry.startedAt.isPresent) {
          entry = entry.copy(
            startWorkflowPosition = Optional.of(order.workflowPosition),
            startedAt = Optional.of(startedAt/*timestamp*/))
        }
        val agentPath = getOrThrow(order.attached)
        val agentUri = eventAndState.state.agentToUris(agentPath).get(0)
        val maybeJobName = eventAndState.state.repo.idToCheckedWorkflow(order.workflowId)
          .flatMap(_.checkedJobName(order.workflowPosition.position))
          .fold(_ => Optional.empty[String], (jobName: WorkflowJob.Name) => Optional.of(jobName.string))
        _idToOrderEntry(orderId) = entry.copy(
          steps =
            (entry.steps.asScala :+
              OrderStepEntry(orderId, order.workflowPosition, agentUri, maybeJobName, order.arguments, startedAt/*timestamp*/)
            ).asJava)

      case event: JOrderProcessed =>
        val order = eventAndState.state.idToOrder.get(orderId)
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
  private val workflowId = TestWorkflowId.asScala
  private val startedAt = Instant.ofEpochMilli(1)
  private val terminatedAt = Instant.ofEpochMilli(2)

  def expectedIdToOrderEntry(agentUris: java.util.List[Uri]): java.util.Map[OrderId, OrderEntry] =
    expectedOrderEntries(agentUris.asScala.toVector).map(o => o.orderId -> o).toMap.asJava

  private def expectedOrderEntries(agentUris: IndexedSeq[Uri]) = {
    val namedValues = Map[String, Value]("KEY" -> StringValue("VALUE")).asJava
    val namedValuesRC0 = (namedValues.asScala ++ NamedValues.rc(0)).asJava
    Vector(
      OrderEntry(
        TestOrderId,
        Optional.empty,
        namedValues,
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
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER"),
            JWorkflowPosition(workflowId /: Position(2)),
            agentUris(0),
            jobName = Optional.empty,
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava),
      OrderEntry(OrderId("ORDER|🥕"),
        Optional.of(OrderId("ORDER")),
        namedValues,
        OrderEntry.Cause.Forked,
        Optional.of(JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 0))),
        Optional.empty,
        Optional.of(startedAt),
        Optional.of(terminatedAt),
        Optional.empty,
        steps = Vector(
          OrderStepEntry(OrderId("ORDER|🥕"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 0)),
            agentUris(0),
            jobName = Optional.empty,
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER|🥕"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🥕" % 1)),
            agentUris(0),
            jobName = Optional.empty,
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava),
      OrderEntry(OrderId("ORDER|🍋"),
        Optional.of(OrderId("ORDER")),
        namedValues,
        OrderEntry.Cause.Forked,
        Optional.of(JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 0))),
        Optional.empty,
        Optional.of(startedAt),
        Optional.of(terminatedAt),
        Optional.empty,
        steps = Vector(
          OrderStepEntry(OrderId("ORDER|🍋"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 0)),
            agentUris(0),
            jobName = Optional.empty,
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput")),
          OrderStepEntry(OrderId("ORDER|🍋"),
            JWorkflowPosition(workflowId /: (Position(1) / "fork+🍋" % 1)),
            agentUris(1),
            jobName = Optional.empty,
            namedValues,
            startedAt,
            Optional.of(terminatedAt),
            Optional.of(namedValues),
            Optional.of(s"stdout: $StdoutOutput"))
        ).asJava))
    }
}
