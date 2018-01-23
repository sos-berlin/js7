package com.sos.jobscheduler.master.tests

import akka.actor.{Actor, ActorSystem, Props}
import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.OrderGeneratorPath
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import io.circe.syntax.EncoderOps
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.Instant.now
import java.time.{Duration, Instant}
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object TestMasterAgent {
  private val TestWorkflowPath = WorkflowPath("/test")
  private val TestJobPath = JobPath("/test")

  /** Für &lt;elememt attribute={stringValue}/>. */
  private implicit def toXmlText(o: IsString): xml.Text = if (o == null) null else xml.Text(o.string)

  def main(args: Array[String]): Unit = {
    lazy val directory =
      temporaryDirectory / "TestMasterAgent" sideEffect { directory ⇒
        println(s"Using -directory=$directory")
        if (!Files.exists(directory))
          createDirectory(directory)
        else {
          println(s"Deleting $directory")
          deleteDirectoryContentRecursively(directory)
        }
      }
    val conf = Conf.parse(args, () ⇒ directory)
    println(s"${conf.agentCount * conf.workflowLength} nodes, ${conf.jobDuration.pretty} each, ${conf.tasksPerJob} tasks/agent, ${conf.agentCount} agents, ${conf.period.pretty}/order")
    run(conf)
  }

  private def run(conf: Conf): Unit = {
    val env = new TestEnvironment(conf.agentPaths, conf.directory)
    withCloser { implicit closer ⇒
      val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(
        configAndData = env.masterDir,
        httpPort = 4444).copy(
        journalSyncOnCommit = conf.syncMaster)))
      injector.instance[Closer].closeWithCloser
      val agents = for (agentPath ← conf.agentPaths) yield {
        env.agentXmlFile(agentPath, TestJobPath).xml =
          <job tasks={conf.tasksPerJob.toString}>
            <params>
              <param name="JOB-VARIABLE" value={s"VALUE-${ agentPath.withoutStartingSlash }"}/>
            </params>
            <script language="shell">{
              if (isWindows) s"""
                 |@echo off
                 |echo Hello
                 |${if (conf.jobDuration.getSeconds > 0) s"ping -n ${conf.jobDuration.getSeconds + 1} 127.0.0.1 >nul" else ""}
                 |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
                 |""".stripMargin
              else s"""
                 |echo Hello ☘️
                 |sleep ${BigDecimal(conf.jobDuration.toMillis, scale = 3).toString}
                 |echo "result=TEST-RESULT-$$SCHEDULER_PARAM_VAR1" >>"$$SCHEDULER_RETURN_VALUES"
                 |""".stripMargin
            }</script>
          </job>
        val agent = RunningAgent(AgentConfiguration.forTest(
            configAndData = Some(env.agentDir(agentPath))).copy(
            journalSyncOnCommit = conf.syncAgent))
          .map { _.closeWithCloser } await 99.s
        env.xmlFile(agentPath).xml = <agent uri={agent.localUri.toString}/>
        agent
      }
      JavaShutdownHook.add("TestMasterAgent") {
        print('\n')
        (for (agent ← agents) yield {
          agent.commandHandler.execute(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(3.seconds)))
          val r = agent.terminated
          agent.close()
          r
        }) await 10.s
        injector.instance[Closer].close()
        Log4j.shutdown()
      } .closeWithCloser

      env.jsonFile(TestWorkflowPath).contentString = makeWorkflow(conf).asJson.toPrettyString
      for (i ← 1 to conf.orderGeneratorCount) {
        env.xmlFile(OrderGeneratorPath(s"/test-$i")).xml =
          <order job_chain={TestWorkflowPath}>
            <params>
              <param name="VARIABLE" value={i.toString}/>
            </params>
            <run_time>
              <period absolute_repeat={(conf.period plusNanos 999999999).getSeconds.toString}/>
            </run_time>
          </order>
      }

      val master = RunningMaster(injector) await 99.s
      val startTime = now
      master.executeCommand(MasterCommand.ScheduleOrdersEvery(10.s.toFiniteDuration))
      injector.instance[ActorSystem].actorOf(Props {
        new Actor {
          injector.instance[StampedKeyedEventBus].subscribe(self, classOf[OrderEvent.OrderAdded])
          injector.instance[StampedKeyedEventBus].subscribe(self, OrderEvent.OrderFinished.getClass)
          context.system.scheduler.schedule(0.seconds, 1.second, self, "PRINT")
          val stopwatch = new Stopwatch
          var added, finished, printedFinished = 0
          var lastDuration: Option[Duration] = None
          var lastLineLength = 0

          def receive = {
            case Stamped(_, KeyedEvent(_, _: OrderEvent.OrderAdded)) ⇒
              added += 1
            case Stamped(_, KeyedEvent(orderId: OrderId, OrderFinished)) ⇒
              lastDuration = Some(now - Instant.parse(orderId.string.substring(orderId.string.indexOf('@') + 1)))
              finished += 1
            case "PRINT" ⇒
              if (finished > printedFinished) {
                val duration = lastDuration map { _.pretty } getOrElse "-"
                val delta = finished - printedFinished
                val diff = s"(diff ${finished - (now - startTime).getSeconds * conf.orderGeneratorCount})"
                val notFinished = added - finished
                val throughput = stopwatch.itemsPerSecondString(finished, "orders")
                val cpu = getOperatingSystemMXBean match {
                  case mx: com.sun.management.OperatingSystemMXBean ⇒ f"  ${(mx.getSystemCpuLoad * 100 + 0.5).toInt}%3d%% CPU"
                  case _ ⇒ ""
                }
                val line = f"$duration%-7s Δ$delta%-3d $diff%-12s  [$notFinished]  $throughput$cpu"
                print("\r" + " " * lastLineLength + "\r" + line)
                lastLineLength = line.length
                printedFinished = finished
              } else {
                print('.')
                lastLineLength += 1
              }
          }
        }
      })
      master.terminated await 365 * 24.h
      for (agent ← agents) agent.commandHandler.execute(AgentCommand.Terminate())
      agents map (_.terminated) await 60.s
    }
  }

  private val PathNames = Stream("🥕", "🍋", "🍊", "🍐", "🍏", "🍓", "🍒") ++ Iterator.from(8).map("🌶".+)

  private def makeWorkflow(conf: Conf): Workflow =
    Workflow(Vector(
      Job(AgentJobPath(conf.agentPaths.head, TestJobPath)),
      ForkJoin(
        for ((agentPath, pathName) ← conf.agentPaths.toVector zip PathNames) yield
          ForkJoin.Branch(
            pathName,
            Workflow(
              for (_ ← 1 to conf.workflowLength) yield
                () @: Job(AgentJobPath(agentPath, TestJobPath))))),
      ExplicitEnd))

  private case class Conf(
    directory: Path,
    agentCount: Int,
    workflowLength: Int,
    tasksPerJob: Int,
    jobDuration: Duration,
    period: Duration,
    orderGeneratorCount: Int,
    syncMaster: Boolean,
    syncAgent: Boolean)
  {
    require(agentCount >= 1)
    require(workflowLength >= 1)
    require(tasksPerJob >= 1)
    require(period > 0.s)
    require(orderGeneratorCount >= 1)

    val agentPaths: Seq[AgentPath] = for (i ← 1 to agentCount) yield AgentPath(s"/AGENT-$i")
  }

  private object Conf {
    def parse(args: collection.Seq[String], directory: () ⇒ Path): Conf =
      CommandLineArguments.parse(args) { a: CommandLineArguments ⇒
        val agentCount = a.as[Int]("-agents=", 1)
        require(agentCount > 0)
        val conf = Conf(
          directory = a.as[Path]("-directory=", directory()),
          agentCount = agentCount,
          workflowLength = a.as[Int]("-nodes-per-agent=", 1),
          tasksPerJob = a.as[Int]("-tasks=", (sys.runtime.availableProcessors + agentCount - 1) / agentCount),
          jobDuration = a.as[Duration]("-job-duration=", 0.s),
          period = a.as[Duration]("-period=", 1.s),
          orderGeneratorCount = a.as[Int]("-orders=", 1),
          syncMaster = a.boolean("-sync-master") || a.boolean("-sync"),
          syncAgent = a.boolean("-sync-agent") || a.boolean("-sync"))
        if (a.boolean("-?") || a.boolean("-help") || a.boolean("--help")) {
          print(usage(conf))
        }
        conf
      }

    def usage(conf: Conf) =
      s"""Usage: -agents=${conf.agentCount}
         |       -nodes-per-agent=${conf.workflowLength}
         |       -tasks=${conf.tasksPerJob}
         |       -job-duration=${conf.jobDuration}
         |       -period=${conf.period}
         |       -orders=${conf.orderGeneratorCount}
         |       [-sync-master -sync-agent -sync]
         |""".stripMargin
  }
}
