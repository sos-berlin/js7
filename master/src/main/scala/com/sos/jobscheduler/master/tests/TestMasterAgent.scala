package com.sos.jobscheduler.master.tests

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.{JobPath, JobnetPath}
import com.sos.jobscheduler.master.Master
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.order.OrderGeneratorPath
import java.time.Duration
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object TestMasterAgent {
  private val TestJobnetPath = JobnetPath("/test")
  private val TestJobPath = JobPath("/test")
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  def main(args: Array[String]): Unit =
    run(Conf.parse(args))

  private def run(conf: Conf): Unit = {
    autoClosing(new TestEnvironment(conf.agentPaths)) { env ⇒
      withCloser { implicit closer ⇒
        val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(
          data = Some(env.masterDir),
          httpPort = 4444).copy(
          journalSyncOnCommit = conf.sync)))
        injector.instance[Closer].closeWithCloser
        for (agentPath ← conf.agentPaths) {
          env.agentXmlFile(agentPath, TestJobPath).xml =
            <job tasks={conf.tasksPerJob.toString}>
              <params>
                <param name="JOB-VARIABLE" value={s"VALUE-${agentPath.withoutStartingSlash}"}/>
              </params>
              <script language="shell">{TestScript}</script>
            </job>
          val agent = new Agent(AgentConfiguration.forTest(
            data = Some(env.agentDir(agentPath))).copy(
              journalSyncOnCommit = conf.sync))
            .closeWithCloser
          env.xmlFile(agentPath).xml = <agent uri={agent.localUri.string}/>
          agent.start()
        }
        env.xmlFile(TestJobnetPath).xml =
          <job_chain>{
            for ((agentPath, i) ← conf.agentPaths.zipWithIndex;
                 j ← 1 to conf.jobnetLength)
              yield <job_chain_node state={s"NODE-${1+i}-$j"} agent={agentPath} job={TestJobPath}/>
            }
            <job_chain_node.end state="END"/>
          </job_chain>
        for (i ← 1 to conf.orderGeneratorCount) {
          env.xmlFile(OrderGeneratorPath(s"/test-$i")).xml =
            <order job_chain={TestJobnetPath} state="NODE-1-1">
              <params>
                <param name="VARIABLE" value={i.toString}/>
              </params>
              <run_time>
                <period absolute_repeat={(conf.period plusNanos 999999999).getSeconds.toString}/>
              </run_time>
            </order>
        }

        val master = injector.instance[Master]
        master.start()
        master.executeCommand(MasterCommand.ScheduleOrdersEvery(60.s))
        sleep((365*24).h)
      }
    }
  }

  private case class Conf(
    agentCount: Int,
    jobnetLength: Int,
    tasksPerJob: Int,
    period: Duration,
    orderGeneratorCount: Int,
    sync: Boolean)
  {
    require(agentCount >= 1)
    require(jobnetLength >= 1)
    require(tasksPerJob >= 1)
    require(period > 0.s)
    require(orderGeneratorCount >= 1)

    val agentPaths: Seq[AgentPath] = for (i ← 1 to agentCount) yield AgentPath(s"/AGENT-$i")
  }

  private object Conf {
    def parse(args: collection.Seq[String]): Conf =
      CommandLineArguments.parse(args) { a: CommandLineArguments ⇒
        val agentCount = a.as[Int]("agents", 1)
        require(agentCount > 0)
        Conf(
          agentCount = agentCount,
          jobnetLength = a.as[Int]("-steps=", 1),
          tasksPerJob = a.as[Int]("-tasks=", sys.runtime.availableProcessors + agentCount - 1 / agentCount),
          period = a.as[Duration]("-period=", 1.s),
          orderGeneratorCount = a.as[Int]("-orders=", 1),
          sync = a.boolean("-sync"))
      }
  }
}
