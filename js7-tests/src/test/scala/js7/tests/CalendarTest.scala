package js7.tests

import com.google.inject.{AbstractModule, Provides}
import java.time.ZoneId
import javax.inject.Singleton
import js7.base.configutils.Configs._
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.{AlarmClock, TestAlarmClock, Timezone}
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.item.VersionId
import js7.data.workflow.Workflow
import js7.tests.CalendarTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.freespec.AnyFreeSpec

final class CalendarTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(calendar)

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }
  override protected def agentModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  private implicit val zone = CalendarTest.zone

  private lazy val versionIdIterator = Iterator.from(1).map(i => VersionId(i.toString))

  "Reject invalid Calendar" in {
    // Falsches Datumsformat
    // Falscher dateOffset
    pending // TODO
  }

  "Add Calendar" in {
    pending // TODO
  }

  "Use Calendar" in {
    pending // TODO
  }

  "Change Calendar" in {
    pending // TODO
  }

  "Delete Workflow and Calendar" in {
    pending // TODO
    // Calendar must be detached from Agent
  }

  private def addWorkflow(workflow: Workflow): Workflow = {
    val v = versionIdIterator.next()
    val w = workflow.withVersion(v)
    directoryProvider.updateVersionedItems(controller, v, Seq(workflow))
    w
  }
}

object CalendarTest
{
  private val agentPath = AgentPath("AGENT")
  private implicit val zone = ZoneId.of("Europe/Mariehamn")
  private val clock = TestAlarmClock(local("2021-10-01T04:00"))

  private val calendar = Calendar(
    CalendarPath("CALENDAR"),
    Timezone("Europe/Mariehamn"),
    dateOffset = 0.h,  // FIXME Test with dateOffset = 6.h
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd")

  // Use this Log4j Clock with the properties
  // -Dlog4j2.Clock=js7.tests.CalendarTestt$CalendarTestLog4jClock -Duser.timezone=Europe/Mariehamn
  final class CalendarTestLog4jClock extends org.apache.logging.log4j.core.util.Clock
  {
    def currentTimeMillis() = clock.epochMilli()
  }
}
