package com.sos.jobscheduler.agent.scheduler.job

import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.Guice
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.scheduler.job.JobKeeperTest._
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.TestAgentActorProvider
import com.sos.jobscheduler.agent.task.{TaskRegister, TaskRegisterActor}
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAny
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.timer.TimerService
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class JobKeeperTest extends FreeSpec {

  "JobKeeper reads some job definitions" in {
    logger.info("START")
    TestAgentActorProvider.provide { provider ⇒
      import provider.agentDirectory
      implicit val agentConfiguration = AgentConfiguration.forTest(agentDirectory)
      val jobDir = agentDirectory / "config" / "live"
      for (i ← (1 to N).par) (jobDir / s"test-$i.job.xml").contentString = TestJobXmlString
      logger.info(s"$N files created, ${TestJobXmlString.length} bytes each")
      withCloser { implicit closer ⇒
        implicit val actorSystem = newActorSystem(getClass.getSimpleName) withCloser { _.terminate() }
        import actorSystem.dispatcher

        val injector = Guice.createInjector(new AgentModule(AgentConfiguration.forTest(agentDirectory)))
        implicit val timeout = Timeout(99.seconds)
        implicit val timerService = injector.instance[TimerService]
        implicit val newTaskRunner = injector.instance[TaskRunner.Factory]
        val taskRegister = new TaskRegister(actorSystem.actorOf(TaskRegisterActor.props(agentConfiguration.killScriptConf, timerService)))

        for (_ ← 1 to 5) {
          val stopwatch = new Stopwatch
          val jobKeeper = actorSystem.actorOf(Props { new JobKeeper(jobDir, newTaskRunner, taskRegister, timerService) })
          val JobKeeper.Output.Ready(jobs) = jobKeeper.ask(JobKeeper.Input.Start)(600.seconds).mapTo[JobKeeper.Output.Ready] await 600.s
          info(stopwatch.itemsPerSecondString(N, "jobs"))
          assert(jobs.size == N)
          actorSystem.stop(jobKeeper)
        }
        actorSystem.terminate() await 99.s
      }
    }
  }
}

object JobKeeperTest {
  private val logger = Logger(getClass)
  private val N = if (sys.props contains "test.speed") 100000 else 100
  private val TestJobXmlString =
    <job>
      <params>{
        for (i ← 1 to 10) yield <param name={s"NAME-$i"} value={"*" * 100}/>
      }</params>
      <script language="shell">SCRIPT</script>
    </job>.toString
}
