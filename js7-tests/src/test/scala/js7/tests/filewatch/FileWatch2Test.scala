package js7.tests.filewatch

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import java.io.File.separator
import java.nio.file.Files.{createDirectories, createDirectory, delete, exists}
import js7.agent.client.AgentClient
import js7.agent.data.event.AgentEvent.AgentReady
import js7.base.auth.Admission
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.monixlike.MonixLikeExtensions.toListL
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isMac
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.controller.ControllerEvent.ControllerShutDown
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedStateEvent, ItemAttachedToMe}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, ItemRevision, UnsignedSimpleItemEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderStarted, OrderStderrWritten}
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Impure, StringConstant}
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatch2Test.*
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProviderForScalaTest

final class FileWatch2Test extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  protected val agentPaths = Seq(aAgentPath, bAgentPath)
  protected val items = Seq(workflow)

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0s
    js7.controller.agent-driver.event-buffer-delay = 0s"""

  // TODO macOS: Waiting for JDK-8293067
  //  https://bugs.java.com/bugdatabase/view_bug?bug_id=8293067
  //  https://github.com/openjdk/jdk/pull/10140
  private val pollTimeout = if isMac && isIntelliJIdea/*because it's slow*/ then "2.5s" else "1s"

  override protected def agentConfig = config"""
    js7.directory-watch.poll-timeout = $pollTimeout
    js7.directory-watch.watch-delay = 1ms
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val aDirectory = directoryProvider.agentEnvs(0).dataDir / "work/a-files"
  private val bDirectory = directoryProvider.agentEnvs(0).dataDir / "work/b-files"

  val orderWatchPath = OrderWatchPath("FILE-WATCH")

  private lazy val aFileWatch = FileWatch(
    orderWatchPath,
    workflow.path,
    aAgentPath,
    StringConstant(aDirectory.toString))
  private lazy val bFileWatch = aFileWatch.copy(directoryExpr = StringConstant(bDirectory.toString))

  private val orderId1 = OrderId("file:FILE-WATCH:1")
  private val orderId2 = OrderId("file:FILE-WATCH:2")
  private val orderId3 = OrderId("file:FILE-WATCH:3")
  private val orderId4 = OrderId("file:FILE-WATCH:4")
  private val orderId5 = OrderId("file:FILE-WATCH:5")
  private val orderId6 = OrderId("file:FILE-WATCH:6")
  private val orderId7 = OrderId("file:FILE-WATCH:7")

  "A sequence of directory changes" in:
    createDirectories(aDirectory)
    val initialFile = aDirectory / "1"
    initialFile := ""
    val initialOrderId = orderId1

    directoryProvider.runController() { controller =>
      controller.api.updateUnsignedSimpleItems(Seq(aFileWatch)).await(99.s).orThrow
      // OrderWatch will be attached to the agent after next restart
    }

    directoryProvider.runController(dontWaitUntilReady = true) { controller =>
      import controller.eventWatch
      import controller.eventWatch.await

      directoryProvider.runAgents(Seq(bAgentPath)) { _ =>
        directoryProvider.runAgents(Seq(aAgentPath)) { _ =>
          await[ItemAttached](_.event.key == orderWatchPath)
          TestJob.continue(2)
          await[OrderFinished](_.key == initialOrderId)
          await[OrderDeleted](_.key == initialOrderId)
          assert(!exists(initialFile))

          locally:
            val orderId = orderId2
            val file = aDirectory / "2"
            file := ""
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))

          assert(!TestJob.semaphore.flatMap(_.tryAcquire).await(99.s))
          aDirectory / "3" := ""
          await[OrderStarted](_.key == orderId3)
        }

        // RESTART WATCHING AGENT WHILE A FILE EXISTS
        directoryProvider.runAgents(Seq(aAgentPath)) { agents =>
          val aAgent = agents.head
          TestJob.continue(2)
          await[OrderFinished](_.key == orderId3)
          // Agent must detect the file deletion after restart to allow the order to be removed:
          await[OrderDeleted](_.key == orderId3)
          assert(!exists(aDirectory / "3"))

          locally:
            // CHANGE DIRECTORY OF FILE ORDER SOURCE
            val orderId = orderId4
            val file = aDirectory / "4"
            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("4"))
            await[OrderStarted](_.key == orderId)
            createDirectory(bDirectory)
            val beforeAttached = eventWatch.lastAddedEventId
            controller.api.updateUnsignedSimpleItems(Seq(bFileWatch)).await(99.s).orThrow
            await[ItemAttached](ke => ke.event.key == orderWatchPath && ke.event.itemRevision == Some(ItemRevision(1)),
              after = beforeAttached)
            // The OrderWatch watches now the bDirectory, but the running Order points to aDirectory.
            // bDirectory does not contain the file
            await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("4"))
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))

          locally:
            val orderId = orderId5
            val file = bDirectory / "5"
            file := ""
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))

          locally:
            // DELETE AND RECREATE FILE WHILE ORDER IS RUNNING, YIELDING A SECOND ORDER
            val orderId = orderId6
            val file = bDirectory / "6"
            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"))
            await[OrderStarted](_.key == orderId)

            TestJob.continue(2)
            val vanished = await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("6"))
              .head.eventId

            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("6"), after = vanished)

            TestJob.continue(2)
            val firstRemoved = await[OrderDeleted](_.key == orderId).head.eventId

            await[OrderStarted](_.key == orderId, after = firstRemoved)
            await[OrderFinished](_.key == orderId, after = firstRemoved)
            await[OrderDeleted](_.key == orderId, after = firstRemoved)
            assert(!exists(file))

          locally:
            // DELETE DIRECTORY
            // The DirectoryWatch keeps observing the directory because only the name is deleted.
            // So it periodically rereads the directory and restarts the watch.
            val orderId = orderId7
            val file = bDirectory / "7"
            delete(bDirectory)
            sleep(500.ms)
            createDirectory(bDirectory)
            file := ""
            await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("7"))
            TestJob.continue(2)
            await[OrderFinished](_.key == orderId)
            await[OrderDeleted](_.key == orderId)
            assert(!exists(file))
          val client = AgentClient(
            Admission(
              aAgent.localUri,
              directoryProvider.agentEnvs.head.controllerUserAndPassword))(
            aAgent.actorSystem)
          checkAgentEvents(client)
        }
      }

      checkControllerEvents(eventWatch.keyedEvents[Event](after = EventId.BeforeFirst))
    }

  private def checkControllerEvents(keyedEvents: Seq[AnyKeyedEvent]): Unit =
    val filteredLeyedEvents = keyedEvents
      .filter(_
        .event match {
          case _: ControllerShutDown => true
          case e: ItemAttachedStateEvent =>
            !e.key.isInstanceOf[AgentPath] && !e.key.isInstanceOf[SubagentId]
          case _: BasicItemEvent => true
          case e: UnsignedSimpleItemEvent => e.key.isInstanceOf[OrderWatchPath]
          case _: OrderAdded => true
          case _: OrderStarted => true
          case _: OrderStderrWritten => true
          case _: OrderFinished => true
          case _: OrderDeleted => true
          case _ => false
        })
    assert(filteredLeyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: UnsignedSimpleItemAdded(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),
      NoKey <-: ItemAttachable(orderWatchPath, aAgentPath),
      NoKey <-: ControllerShutDown,
      NoKey <-: ItemAttached(orderWatchPath, Some(ItemRevision(0)), aAgentPath),
      orderId1 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory${separator}1"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("1")))),
      NoKey <-: ItemAttached(workflow.id, None, bAgentPath),
      orderId1 <-: OrderStarted,
      orderId1 <-: OrderStderrWritten(s"Deleted $aDirectory${separator}1\n"),
      orderId1 <-: OrderFinished(),
      orderId1 <-: OrderDeleted,

      orderId2 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory${separator}2"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("2")))),
      orderId2 <-: OrderStarted,
      orderId2 <-: OrderStderrWritten(s"Deleted $aDirectory${separator}2\n"),
      orderId2 <-: OrderFinished(),
      orderId2 <-: OrderDeleted,

      orderId3 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory${separator}3"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("3")))),
      orderId3 <-: OrderStarted,
      orderId3 <-: OrderStderrWritten(s"Deleted $aDirectory${separator}3\n"),
      orderId3 <-: OrderFinished(),
      orderId3 <-: OrderDeleted,

      orderId4 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$aDirectory${separator}4"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("4")))),
      orderId4 <-: OrderStarted,
      NoKey <-: UnsignedSimpleItemChanged(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
      NoKey <-: ItemAttachable(orderWatchPath, aAgentPath),
      NoKey <-: ItemAttached(orderWatchPath, Some(ItemRevision(1)), aAgentPath),
      orderId4 <-: OrderStderrWritten(s"Deleted $aDirectory${separator}4\n"),
      orderId4 <-: OrderFinished(),
      orderId4 <-: OrderDeleted,

      orderId5 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory${separator}5"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("5")))),
      orderId5 <-: OrderStarted,
      orderId5 <-: OrderStderrWritten(s"Deleted $bDirectory${separator}5\n"),
      orderId5 <-: OrderFinished(),
      orderId5 <-: OrderDeleted,

      orderId6 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory${separator}6"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory${separator}6\n"),
      orderId6 <-: OrderFinished(),
      orderId6 <-: OrderDeleted,
      // And again
      orderId6 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory${separator}6"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("6")))),
      orderId6 <-: OrderStarted,
      orderId6 <-: OrderStderrWritten(s"Deleted $bDirectory${separator}6\n"),
      orderId6 <-: OrderFinished(),
      orderId6 <-: OrderDeleted,

      orderId7 <-: OrderAdded(workflow.id,
        Map(
          FileArgumentName -> StringValue(s"$bDirectory${separator}7"),
          "var" -> StringValue("VAR")),
        externalOrderKey = Some(ExternalOrderKey(orderWatchPath, ExternalOrderName("7")))),
      orderId7 <-: OrderStarted,
      orderId7 <-: OrderStderrWritten(s"Deleted $bDirectory${separator}7\n"),
      orderId7 <-: OrderFinished(),
      orderId7 <-: OrderDeleted))

  private def checkAgentEvents(client: AgentClient): Unit =
    client.login().await(99.s)
    val keyedEvents = client
      .agentEventStream:
        EventRequest.singleClass[Event](after = EventId.BeforeFirst, timeout = Some(0.s))
      .await(99.s)
      .orThrow
      .map(_.value)
      .flatMap(ke => Stream.emit(ke.event)
        .collect {
          case e: AgentReady => e.copy(
            timezone = "Europe/Berlin",
            totalRunningTime = 1.s,
            platformInfo = Some(PlatformInfo.test))
          case e: InventoryItemEvent if e.key.isInstanceOf[OrderWatchPath] => e
          case e: OrderWatchEvent => e
        }
        .map(e => ke.key <~: e))
      .toListL.await(99.s)
    assert(keyedEvents == Seq[AnyKeyedEvent](
      NoKey <-: AgentReady("Europe/Berlin", 1.s, Some(PlatformInfo.test)),
      NoKey <-: ItemAttachedToMe(aFileWatch.copy(itemRevision = Some(ItemRevision(0)))),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("1"),
        OrderId("file:FILE-WATCH:1"),
        Map("file" -> StringValue(s"$aDirectory${separator}1"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("1")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("2"),
        OrderId("file:FILE-WATCH:2"),
        Map("file" -> StringValue(s"$aDirectory${separator}2"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("2")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("3"),
        OrderId("file:FILE-WATCH:3"),
        Map("file" -> StringValue(s"$aDirectory${separator}3"))),
      NoKey <-: AgentReady("Europe/Berlin", 1.s, Some(PlatformInfo.test)),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("3")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("4"),
        OrderId("file:FILE-WATCH:4"),
        Map("file" -> StringValue(s"$aDirectory${separator}4"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("4")),
      NoKey <-: ItemAttachedToMe(bFileWatch.copy(itemRevision = Some(ItemRevision(1)))),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("5"),
        OrderId("file:FILE-WATCH:5"),
        Map("file" -> StringValue(s"$bDirectory${separator}5"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("5")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory${separator}6"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("6")),
      // and again
      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("6"),
        OrderId("file:FILE-WATCH:6"),
        Map("file" -> StringValue(s"$bDirectory${separator}6"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("6")),

      orderWatchPath <-: ExternalOrderArised(ExternalOrderName("7"),
        OrderId("file:FILE-WATCH:7"),
        Map("file" -> StringValue(s"$bDirectory${separator}7"))),
      orderWatchPath <-: ExternalOrderVanished(ExternalOrderName("7"))))


object FileWatch2Test:
  private val aAgentPath = AgentPath("AGENT-A")
  private val bAgentPath = AgentPath("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      TestJob.execute(bAgentPath),
      DeleteFileJob.execute(bAgentPath),
      TestJob.execute(bAgentPath)),
    orderPreparation = OrderPreparation(
      OrderParameterList(
        Seq(
          OrderParameter.Final("var",
            Impure/*force impure (non-constant) early evaluation*/(StringConstant("VAR")))),
        allowUndeclared = true)))


  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
