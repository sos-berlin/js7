package com.sos.jobscheduler.tests

import akka.actor.{Actor, ActorSystem, Props}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.DecimalPrefixes
import com.sos.jobscheduler.base.utils.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.utils.JavaShutdownHook
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.expression.Expression.{Equal, LastReturnCode, NumericConstant, Or}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, Fork, If}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object TestMasterAgent
{
  private val TestWorkflowPath = WorkflowPath("/test")
  private val TestExecutablePath = ExecutablePath("/test")
  private val StdoutRowSize = 1000

  def main(args: Array[String]): Unit = {
    lazy val directory =
      temporaryDirectory / "TestMasterAgent" sideEffect { directory =>
        println(s"Using -directory=$directory")
        if (!Files.exists(directory))
          createDirectory(directory)
        else {
          println(s"Deleting $directory")
          deleteDirectoryContentRecursively(directory)
        }
      }
    val conf = Conf.parse(args, () => directory)
    println(s"${conf.agentCount * conf.workflowLength} jobs/agent, ${conf.jobDuration.pretty} each, ${conf.tasksPerJob} tasks/agent, ${conf.agentCount} agents, ${conf.period.pretty}/order")
    try run(conf)
    finally Log4j.shutdown()
  }

  private def run(conf: Conf): Unit = {
    autoClosing(new DirectoryProvider(conf.agentRefPaths, makeWorkflow(conf) :: Nil, useDirectory = Some(conf.directory))) { env =>
      env.master.config / "master.conf" ++= "jobscheduler.webserver.auth.loopback-is-public = on\n"
      env.agents foreach { _.config / "agent.conf" ++= "jobscheduler.webserver.auth.loopback-is-public = on\n" }
      withCloser { implicit closer =>
        for (agentRefPath <- conf.agentRefPaths) {
          TestExecutablePath.toFile(env.agentToTree(agentRefPath).config / "executables").writeExecutable(
              if (isWindows) s"""
                 |@echo off
                 |echo Hello
                 |${if (conf.jobDuration.toSeconds > 0) s"ping -n ${conf.jobDuration.toSeconds + 1} 127.0.0.1 >nul" else ""}
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
                 |""".stripMargin)
        }

        env.runAgents() { agents =>
          env.runMaster() { master =>
            JavaShutdownHook.add("TestMasterAgent") {
              print('\n')
              (for (agent <- agents) yield {
                agent.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(3.seconds)))
                val r = agent.terminated
                agent.close()
                r
              }) await 10.s
              master.injector.instance[Closer].close()
              Log4j.shutdown()
            } .closeWithCloser

            val startTime = Timestamp.now
            Scheduler.global.scheduleWithFixedDelay(0.seconds, conf.period) {
              for (i <- 1 to conf.orderGeneratorCount) {
                val at = Timestamp.now
                master.addOrder(FreshOrder(OrderId(s"test-$i@$at"), TestWorkflowPath, Some(at))).runAsyncAndForget  // No error checking
              }
            }
            master.injector.instance[ActorSystem].actorOf(Props {
              new Actor {
                master.injector.instance[StampedKeyedEventBus].subscribe(self, classOf[OrderEvent.OrderAdded])
                master.injector.instance[StampedKeyedEventBus].subscribe(self, OrderEvent.OrderFinished.getClass)
                context.system.scheduler.schedule(0.seconds, 1.second, self, "PRINT")
                val stopwatch = new Stopwatch
                var added, finished, printedFinished = 0
                var lastDuration: Option[FiniteDuration] = None
                var lastLineLength = 0

                def receive = {
                  case Stamped(_, _, KeyedEvent(_, _: OrderEvent.OrderAdded)) =>
                    added += 1
                  case Stamped(_, _, KeyedEvent(orderId: OrderId, OrderFinished)) =>
                    lastDuration = Some(Timestamp.now - Timestamp.parse(orderId.string.substring(orderId.string.indexOf('@') + 1)))
                    finished += 1
                  case "PRINT" =>
                    if (finished > printedFinished) {
                      val duration = lastDuration.fold("-")(_.pretty)
                      val delta = finished - printedFinished
                      val diff = s"(diff ${finished - (Timestamp.now - startTime).toSeconds * conf.orderGeneratorCount})"
                      val notFinished = added - finished
                      val throughput = stopwatch.itemsPerSecondString(finished, "orders")
                      val cpu = getOperatingSystemMXBean match {
                        case mx: com.sun.management.OperatingSystemMXBean => f"  ${(mx.getSystemCpuLoad * 100 + 0.5).toInt}%3d%% CPU"
                        case _ => ""
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
            for (agent <- agents) agent.executeCommand(AgentCommand.Terminate())
          }
        }
      }
    }
  }

  private val PathNames = Stream("🥕", "🍋", "🍊", "🍐", "🍏", "🍓", "🍒") ++ Iterator.from(8).map("🌶".+)
  private def testJob(conf: Conf, agentRefPath: AgentRefPath) =
    WorkflowJob(agentRefPath, TestExecutablePath,
      Map("JOB-VARIABLE" -> s"VALUE-${agentRefPath.withoutStartingSlash}"),
      taskLimit = conf.tasksPerJob)

  private def makeWorkflow(conf: Conf): Workflow =
    Workflow.of(TestWorkflowPath,
      Execute(testJob(conf, conf.agentRefPaths.head)),
      Fork.checked(
        for ((agentRefPath, pathName) <- conf.agentRefPaths.toVector zip PathNames) yield
          Fork.Branch(
            pathName,
            Workflow(
              WorkflowPath("/TestMasterAgent") ~ "1",
              Vector.fill(conf.workflowLength) { Execute(WorkflowJob(agentRefPath, TestExecutablePath)) })))
        .orThrow,
      If(Or(Equal(LastReturnCode, NumericConstant(0)), Equal(LastReturnCode, NumericConstant(0))),
        thenWorkflow = Workflow.of(Execute(testJob(conf, conf.agentRefPaths.head))),
        elseWorkflow = Some(Workflow.of(Execute(testJob(conf, conf.agentRefPaths.head))))))

  private case class Conf(
    directory: Path,
    agentCount: Int,
    workflowLength: Int,
    tasksPerJob: Int,
    jobDuration: FiniteDuration,
    stdoutSize: Int,
    period: FiniteDuration,
    orderGeneratorCount: Int)
  {
    require(agentCount >= 1)
    require(workflowLength >= 1)
    require(tasksPerJob >= 1)
    require(period > 0.s)
    require(orderGeneratorCount >= 1)

    val agentRefPaths: Seq[AgentRefPath] = for (i <- 1 to agentCount) yield AgentRefPath(s"/AGENT-$i")
  }

  private object Conf {
    def parse(args: collection.Seq[String], directory: () => Path): Conf =
      CommandLineArguments.parse(args) { a: CommandLineArguments =>
        val agentCount = a.as[Int]("-agents=", 1)
        require(agentCount > 0)
        val conf = Conf(
          directory = a.as[Path]("-directory=", directory()),
          agentCount = agentCount,
          workflowLength = a.as[Int]("-jobs-per-agent=", 1),
          tasksPerJob = a.as[Int]("-tasks=", (sys.runtime.availableProcessors + agentCount - 1) / agentCount),
          jobDuration = a.as[FiniteDuration]("-job-duration=", 0.s),
          stdoutSize = a.as("-stdout-size=", StdoutRowSize)(o => DecimalPrefixes.toInt(o).orThrowWithoutStacktrace),
          period = a.as[FiniteDuration]("-period=", 1.s),
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
