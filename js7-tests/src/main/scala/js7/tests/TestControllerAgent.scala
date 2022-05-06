package js7.tests

import akka.actor.{Actor, Props}
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.base.convert.AsJava.StringAsPath
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, temporaryDirectory}
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.{Log4j, Logger}
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.Closer.withCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SideEffect.ImplicitSideEffect
import js7.base.utils.{Closer, DecimalPrefixes}
import js7.common.commandline.CommandLineArguments
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.utils.JavaShutdownHook
import js7.data.agent.AgentPath
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.expression.Expression.{Equal, LastReturnCode, NumericConstant, Or, StringConstant}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork, If}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.StampedKeyedEventBus
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object TestControllerAgent
{
  private val TestWorkflowPath = WorkflowPath("test")
  private val TestPathExecutable = RelativePathExecutable("test")
  private val StdoutRowSize = 1000
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    lazy val directory =
      temporaryDirectory / "TestControllerAgent" sideEffect { directory =>
        println(s"Using --directory=$directory")
        if (!Files.exists(directory))
          createDirectory(directory)
        else {
          println(s"Deleting $directory")
          deleteDirectoryContentRecursively(directory)
        }
      }
    val conf = Conf.parse(args.toIndexedSeq, () => directory)
    println(s"${conf.agentCount * conf.workflowLength} jobs/agent, ${conf.jobDuration.pretty} each, ${conf.tasksPerJob} tasks/agent, ${conf.agentCount} agents, ${conf.period.pretty}/order")
    try run(conf)
    finally Log4j.shutdown()
  }

  private def run(conf: Conf): Unit = {
    autoClosing(new DirectoryProvider(conf.agentPaths, makeWorkflow(conf) :: Nil, useDirectory = Some(conf.directory))) { env =>
      env.controller.configDir / "controller.conf" ++= "js7.web.server.auth.loopback-is-public = on\n"
      env.agents foreach { _.configDir / "agent.conf" ++= "js7.web.server.auth.loopback-is-public = on\n" }
      withCloser { implicit closer =>
        for (agentPath <- conf.agentPaths) {
          TestPathExecutable.toFile(env.agentToTree(agentPath).configDir / "executables").writeUtf8Executable(
              if (isWindows) s"""
                 |@echo off
                 |echo Hello
                 |${(conf.jobDuration.toSeconds > 0) ?? s"ping -n ${conf.jobDuration.toSeconds + 1} 127.0.0.1 >nul"}
                 |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
                 |""".stripMargin
              else s"""#!/usr/bin/env bash
                 |set -euo pipefail
                 |echo Hello ☘️
                 |for a in {1..${conf.stdoutSize / StdoutRowSize}}; do
                 |  echo "»${"x" * (StdoutRowSize - 2)}«"
                 |done
                 |sleep ${BigDecimal(conf.jobDuration.toMillis, scale = 3).toString}
                 |echo "result=TEST-RESULT-$$SCHEDULER_PARAM_VAR1" >>"$$SCHEDULER_RETURN_VALUES"
                 |""".stripMargin)
        }

        env.runAgents() { agents =>
          env.runController() { controller =>
            JavaShutdownHook.add("TestControllerAgent") {
              print('\n')
              (for (agent <- agents) yield {
                agent.executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
                val r = agent.terminated
                agent.close()
                r
              }) await 10.s
              controller.injector.instance[Closer].close()
              Log4j.shutdown()
            } .closeWithCloser

            val startTime = Timestamp.now
            Scheduler.traced.scheduleWithFixedDelay(0.s, conf.period) {
              for (i <- 1 to conf.orderGeneratorCount) {
                val at = Timestamp.now
                controller
                  .addOrder(FreshOrder(OrderId(s"test-$i@$at"), TestWorkflowPath, scheduledFor = Some(at)))
                  .map(_.orThrow)
                  .onErrorRecover { case t: Throwable =>
                    logger.error(s"addOrder failed: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                  }
                  .runAsyncAndForget
              }
            }
            controller.actorSystem.actorOf(Props {
              new Actor {
                controller.injector.instance[StampedKeyedEventBus].subscribe(self, classOf[OrderEvent.OrderAdded])
                controller.injector.instance[StampedKeyedEventBus].subscribe(self, OrderEvent.OrderFinished.getClass)
                context.system.scheduler.scheduleWithFixedDelay(0.s, 1.s, self, "PRINT")
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
            controller.terminated await 365 * 24.h
            controller.close()
            for (agent <- agents) agent.executeCommandAsSystemUser(AgentCommand.ShutDown())
          }
        }
      }
    }
  }

  private val PathNames = LazyList("🥕", "🍋", "🍊", "🍐", "🍏", "🍓", "🍒") ++ Iterator.from(8).map("🌶".+)
  private def testJob(conf: Conf, agentPath: AgentPath) =
    WorkflowJob(agentPath, TestPathExecutable,
      Map("JOB-VARIABLE" -> StringConstant(s"VALUE-$agentPath")),
      parallelism = conf.tasksPerJob)

  private def makeWorkflow(conf: Conf): Workflow =
    Workflow.of(TestWorkflowPath,
      Execute(testJob(conf, conf.agentPaths.head)),
      Fork.checked(
        for ((agentPath, pathName) <- conf.agentPaths.toVector zip PathNames) yield
          Fork.Branch(
            pathName,
            Workflow(
              WorkflowPath("TestControllerAgent") ~ "1",
              Vector.fill(conf.workflowLength) { Execute(WorkflowJob(agentPath, TestPathExecutable)) })))
        .orThrow,
      If(Or(Equal(LastReturnCode, NumericConstant(0)), Equal(LastReturnCode, NumericConstant(0))),
        thenWorkflow = Workflow.of(Execute(testJob(conf, conf.agentPaths.head))),
        elseWorkflow = Some(Workflow.of(Execute(testJob(conf, conf.agentPaths.head))))))

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
    require(period.isPositive)
    require(orderGeneratorCount >= 1)

    val agentPaths: Seq[AgentPath] = for (i <- 1 to agentCount) yield AgentPath(s"AGENT-$i")
  }

  private object Conf {
    def parse(args: Seq[String], directory: () => Path): Conf =
      CommandLineArguments.parse(args) { a: CommandLineArguments =>
        val agentCount = a.as[Int]("--agents=", 1)
        require(agentCount > 0)
        val conf = Conf(
          directory = a.as[Path]("--directory=", directory()),
          agentCount = agentCount,
          workflowLength = a.as[Int]("--jobs-per-agent=", 1),
          tasksPerJob = a.as[Int]("--tasks=", (sys.runtime.availableProcessors + agentCount - 1) / agentCount),
          jobDuration = a.as[FiniteDuration]("--job-duration=", 0.s),
          stdoutSize = a.as("--stdout-size=", StdoutRowSize)(o => DecimalPrefixes.toInt(o).orThrowWithoutStacktrace),
          period = a.as[FiniteDuration]("--period=", 1.s),
          orderGeneratorCount = a.as[Int]("--orders=", 1))
        if (a.boolean("-?") || a.boolean("--help")) {
          print(usage(conf))
        }
        conf
      }

    def usage(conf: Conf) =
      s"""Usage: --agents=${conf.agentCount}
         |       --jobs-per-agent=${conf.workflowLength}
         |       --tasks=${conf.tasksPerJob}
         |       --job-duration=${conf.jobDuration}
         |       --period=${conf.period}
         |       --orders=${conf.orderGeneratorCount}
         |""".stripMargin
  }
}
