package com.sos.jobscheduler.master.tests

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.{JobPath, JobnetPath}
import com.sos.jobscheduler.master.Master
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.order.OrderGeneratorPath

/**
  * @author Joacim Zschimmer
  */
object TestMaster {
  private val AgentPaths = Vector(AgentPath("/test-agent-111"), AgentPath("/test-agent-222"))
  private val TestJobnetPath = JobnetPath("/test")
  private val TestOrderGeneratorPath = OrderGeneratorPath("/test")
  private val TestJobPath = JobPath("/test")
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    autoClosing(new TestEnvironment(AgentPaths)) { env ⇒
      withCloser { implicit closer ⇒
        val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(data = Some(env.masterDir), httpPort = 4444)))
        injector.instance[Closer].closeWithCloser
        val agents = for (agentPath ← AgentPaths) yield {
          env.agentXmlFile(agentPath, TestJobPath).xml =
            <job>
              <params>
                <param name="JOB-VARIABLE" value={s"VALUE-${agentPath.withoutStartingSlash}"}/>
              </params>
              <script language="shell">{TestScript}</script>
            </job>
          val agent = new Agent(AgentConfiguration.forTest().copy(
            dataDirectory = Some(env.agentDir(agentPath))))
            .closeWithCloser
          agent.start()
          agent
        }
        env.xmlFile(TestJobnetPath).xml =
          <job_chain>
            <job_chain_node state="100" agent={AgentPaths(0)} job={TestJobPath}/>
            <job_chain_node state="200" agent={AgentPaths(1)} job={TestJobPath}/>
            <job_chain_node.end state="END"/>
          </job_chain>
        env.xmlFile(TestOrderGeneratorPath).xml =
          <order job_chain={TestJobnetPath} state="100">
            <params>
              <param name="VARIABLE" value="VALUE"/>
            </params>
            <run_time>
              <period absolute_repeat="1"/>
            </run_time>
          </order>
        for ((agentPath, agent) ← AgentPaths zip agents) {
          env.xmlFile(agentPath).xml = <agent uri={agent.localUri.string}/>
        }
        val master = injector.instance[Master]
        master.start()
        master.executeCommand(MasterCommand.ScheduleOrdersEvery(60.s))
        sleep((365*24).h)
      }
    }
  }
}
