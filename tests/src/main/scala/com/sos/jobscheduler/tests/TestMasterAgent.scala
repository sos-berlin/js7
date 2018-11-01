package com.sos.jobscheduler.tests

import akka.actor.{Actor, ActorSystem, Props}
import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.utils.DecimalPrefixes
import com.sos.jobscheduler.base.utils.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.auth.SecretStringGenerator.newSecretString
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Closer
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{Equal, NumericConstant, Or, OrderReturnCode}
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, If, Job}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.scheduledorder.ScheduledOrderGeneratorPath
import com.sos.jobscheduler.master.tests.TestEnvironment
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import java.time.Instant.now
import java.time.{Duration, Instant}
import monix.execution.Scheduler.Implicits.global
import scala.collection.immutable.Seq
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object TestMasterAgent {
  private val TestWorkflowPath = WorkflowPath("/test")
  private val TestJobPath = JobPath("/test")
  private val StdoutRowSize = 1000

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
    println(s"${conf.agentCount * conf.workflowLength} jobs/agent, ${conf.jobDuration.pretty} each, ${conf.tasksPerJob} tasks/agent, ${conf.agentCount} agents, ${conf.period.pretty}/order")
    try run(conf)
    finally Log4j.shutdown()
  }

  private def run(conf: Conf): Unit = {
    val env = new TestEnvironment(conf.agentPaths, conf.directory)
    val agentToPassword = conf.agentPaths.map(_ → newSecretString()).toMap
    withCloser { implicit closer ⇒
      env.masterDir / "config" / "private" / "private.conf" :=
        s"""jobscheduler.webserver.auth.loopback-is-public = on
            |jobscheduler.auth.agents {
            |  # Passwords for each Agent path (user name is the configured Master ID or "Master")
            |""".stripMargin +
         agentToPassword.map(o ⇒ s"""  "${o._1.string}" = "${o._2.string}"\n""").mkString +
         "}\n"
      val masterConfiguration = MasterConfiguration.forTest(configAndData = env.masterDir, httpPort = Some(4444))
      val injector = Guice.createInjector(new MasterModule(masterConfiguration.copy(
        config = masterConfiguration.config)))
      injector.instance[Closer].closeWithCloser
      val agents = for (agentPath ← conf.agentPaths) yield {
        env.agentDir(agentPath) / "config" / "private" / "private.conf" :=
          s"""jobscheduler.auth.users.Master = "plain:${agentToPassword(agentPath).string}"
             |""".stripMargin
        env.agentFile(agentPath, TestJobPath, SourceType.Json) := JobConfiguration(
          JobPath.NoId,
          JobScript(
            if (isWindows) s"""
               |@echo off
               |echo Hello
               |${if (conf.jobDuration.getSeconds > 0) s"ping -n ${conf.jobDuration.getSeconds + 1} 127.0.0.1 >nul" else ""}
               |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
               |""".stripMargin
            else s"""
               |/usr/bin/env bash
               |set -e
               |echo Hello ☘️
               |for a in {1..${conf.stdoutSize / StdoutRowSize}}; do
               |  echo "»${"x" * (StdoutRowSize - 2)}«"
               |done
               |sleep ${BigDecimal(conf.jobDuration.toMillis, scale = 3).toString}
               |echo "result=TEST-RESULT-$$SCHEDULER_PARAM_VAR1" >>"$$SCHEDULER_RETURN_VALUES"
               |""".stripMargin),
          Map("JOB-VARIABLE" → s"VALUE-${agentPath.withoutStartingSlash}"),
          taskLimit = conf.tasksPerJob)
        val agent = RunningAgent.startForTest(
          AgentConfiguration.forTest(configAndData = env.agentDir(agentPath))
        ) map { _.closeWithCloser } await 99.s
        env.file(agentPath, SourceType.Json) := Agent(AgentPath.NoId, uri = agent.localUri.toString)
        agent
      }
      JavaShutdownHook.add("TestMasterAgent") {
        print('\n')
        (for (agent ← agents) yield {
          agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(3.seconds)))
          val r = agent.terminated
          agent.close()
          r
        }) await 10.s
        injector.instance[Closer].close()
        Log4j.shutdown()
      } .closeWithCloser

      env.writeJson(TestWorkflowPath, makeWorkflow(conf))
      for (i ← 1 to conf.orderGeneratorCount) {
        env.file(ScheduledOrderGeneratorPath(s"/test-$i"), SourceType.Xml).xml =
          <order job_chain={TestWorkflowPath.string}>
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
      master.executeCommandAsSystemUser(MasterCommand.ScheduleOrdersEvery(10.s.toFiniteDuration)) await 99.s
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
            case Stamped(_, _, KeyedEvent(_, _: OrderEvent.OrderAdded)) ⇒
              added += 1
            case Stamped(_, _, KeyedEvent(orderId: OrderId, OrderFinished)) ⇒
              lastDuration = Some(now - Instant.parse(orderId.string.substring(orderId.string.indexOf('@') + 1)))
              finished += 1
            case "PRINT" ⇒
              if (finished > printedFinished) {
                val duration = lastDuration.fold("-")(_.pretty)
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
      master.close()
      for (agent ← agents) agent.executeCommand(AgentCommand.Terminate())
      agents map (_.terminated) await 60.s
      agents foreach (_.close())
    }
  }

  private val PathNames = Stream("🥕", "🍋", "🍊", "🍐", "🍏", "🍓", "🍒") ++ Iterator.from(8).map("🌶".+)

  private def makeWorkflow(conf: Conf): Workflow =
    Workflow.of(
      Job(TestJobPath, conf.agentPaths.head),
      ForkJoin(
        for ((agentPath, pathName) ← conf.agentPaths.toVector zip PathNames) yield
          ForkJoin.Branch(
            pathName,
            Workflow(
              WorkflowPath("/TestMasterAgent") % "1",
              Vector.fill(conf.workflowLength) { Job(TestJobPath, agentPath) }))),
      If(Or(Equal(OrderReturnCode, NumericConstant(0)), Equal(OrderReturnCode, NumericConstant(0))),
        Workflow.of(Job(TestJobPath, conf.agentPaths.head)),
        Some(Workflow.of(Job(TestJobPath, conf.agentPaths.head)))))

  private case class Conf(
    directory: Path,
    agentCount: Int,
    workflowLength: Int,
    tasksPerJob: Int,
    jobDuration: Duration,
    stdoutSize: Int,
    period: Duration,
    orderGeneratorCount: Int)
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
          workflowLength = a.as[Int]("-jobs-per-agent=", 1),
          tasksPerJob = a.as[Int]("-tasks=", (sys.runtime.availableProcessors + agentCount - 1) / agentCount),
          jobDuration = a.as[Duration]("-job-duration=", 0.s),
          stdoutSize = a.as("-stdout-size=", StdoutRowSize)(o ⇒ DecimalPrefixes.toInt(o)),
          period = a.as[Duration]("-period=", 1.s),
          orderGeneratorCount = a.as[Int]("-orders=", 1))
        if (a.boolean("-?") || a.boolean("-help") || a.boolean("--help")) {
          print(usage(conf))
        }
        conf
      }

    def usage(conf: Conf) =
      s"""Usage: -agents=${conf.agentCount}
         |       -jobs-per-agent=${conf.workflowLength}
         |       -tasks=${conf.tasksPerJob}
         |       -job-duration=${conf.jobDuration}
         |       -period=${conf.period}
         |       -orders=${conf.orderGeneratorCount}
         |""".stripMargin
  }
}
