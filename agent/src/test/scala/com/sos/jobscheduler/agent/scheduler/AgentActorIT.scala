package com.sos.jobscheduler.agent.scheduler

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Guice}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commandresponses.Response
import com.sos.jobscheduler.agent.data.commands.{AttachJobnet, AttachOrder, Command, DetachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.scheduler.AgentActorIT._
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.auth.User.Anonymous
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.{ActorEventCollector, StampedKeyedEventBus}
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class AgentActorIT extends FreeSpec {

  private implicit val askTimeout = Timeout(60.seconds)

  for (n ← List(10) ++ (sys.props contains "test.speed" option 1000)) {
    s"AgentActorIT, $n orders" in {
      provideAgentDirectory { dir ⇒
        withCloser { implicit closer ⇒
          val (eventCollector, main) = start(dir)
          val lastEventId = eventCollector.lastEventId
          def execute(command: Command): Future[Response] = {
            val response = Promise[Response]()
            main ! AgentActor.Input.ExternalCommand(MasterUserId, command, response)
            response.future
          }
          (main ? AgentActor.Input.Start).mapTo[AgentActor.Output.Started.type] await 99.s
          execute(RegisterAsMaster) await 99.s
          execute(AttachJobnet(TestJobnet)) await 99.s
          val stopwatch = new Stopwatch
          val orderIdGenerator = for (i ← Iterator from 1) yield OrderId(s"TEST-ORDER-$i")
          val orderIds = Vector.fill(n) { orderIdGenerator.next() }
          (for (orderId ← orderIds) yield {
            val addOrder = AttachOrder(Order(
              orderId,
              NodeKey(TestJobnet.path, NodeId("100")),
              Order.Waiting,
              Map("a" → "A")))
            execute(addOrder)
          }) await 99.s

          (for (orderId ← orderIds) yield
            eventCollector.whenForKey[OrderEvent.OrderReady.type](EventRequest.singleClass(after = lastEventId, 1.h), orderId)) await 99.s
          info(stopwatch.itemsPerSecondString(n, "Orders"))
          (for (orderId ← orderIds) yield execute(DetachOrder(orderId))) await 99.s
        }
      }
    }
  }

  private def start(configAndData: Path)(implicit closer: Closer): (EventCollector, ActorRef) = {
    val agentConfiguration = AgentConfiguration.forTest(configAndData = Some(configAndData))
    val actorSystem = newActorSystem(getClass.getSimpleName)
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))
    implicit val agentTaskFactory = injector.instance[AgentTaskFactory]
    implicit val timerService = TimerService(idleTimeout = Some(1.s))
    implicit val keyedEventBus = injector.instance[StampedKeyedEventBus]
    implicit val eventIdGenerator = injector.instance[EventIdGenerator]

    val eventCollector = injector.createChildInjector(new AbstractModule {
      def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000, timeoutLimit = 99.s)
    }).instance[ActorEventCollector]
    val main = actorSystem.actorOf(
      Props { new AgentActor(
        stateDirectory = configAndData / "data",
        jobConfigurationDirectory = configAndData / "config" / "live",
        askTimeout = Timeout(30.seconds),
        syncOnCommit = false) },
      "AgentActor")
    (eventCollector, main)
  }
}

object AgentActorIT {
  private val MasterUserId = Anonymous.id
  private val TestAgentId = AgentPath("/TEST-AGENT")
  private val AJobPath = JobPath("/test")
  private val BJobPath = JobPath("/folder/test")
  private val TestJobnet = Jobnet(
    JobnetPath("/TEST"),
    NodeId("100"),
    List(
      Jobnet.JobNode(NodeId("100"), TestAgentId, AJobPath, onSuccess = NodeId("END"), onFailure = NodeId("FAILED")),
      Jobnet.EndNode(NodeId("FAILED")),
      Jobnet.EndNode(NodeId("END"))))

  private val AScript =
    if (isWindows) """
        |@echo off
        |echo Hej!
        |echo var1=%SCHEDULER_PARAM_VAR1%
        |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
        |""".stripMargin
    else """
        |echo "Hej!"
        |echo "var1=$SCHEDULER_PARAM_VAR1"
        |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
        |""".stripMargin

  def provideAgentDirectory[A](body: Path ⇒ A): A =
    AgentDirectoryProvider.provideAgent2Directory { directory ⇒
      val subdir =  directory / "config" / "live" / "folder"
      createDirectory(subdir)
      (directory / "config" / "live" / "test.job.xml").xml =
        <job tasks="100">
          <params>
            <param name="var1" value="VALUE1"/>
          </params>
          <script language="shell">{AScript}</script>
        </job>
      (subdir / "test.job.xml").xml = <job><script language="shell">FOLDER/TEST</script></job>
      body(directory)
  }
}
