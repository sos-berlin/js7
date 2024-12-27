package js7.agent.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Resource}
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand.*
import js7.agent.web.CommandWebServerTest.*
import js7.base.auth.{Admission, SimpleUser}
import js7.base.configutils.Configs.*
import js7.base.test.OurAsyncTestSuite
import js7.base.web.Uri
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.pekkoutils.Pekkos.actorSystemResource
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerRunId
import js7.data.event.{EventId, JournalId}
import js7.data.order.OrderId
import js7.subagent.SubagentSession
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.decodeRequest

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServerTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val n = 1_000 //1_000_000
  private lazy val orderIds = (for i <- 1 to n yield OrderId(s"A-MEDIUM-LONG-ORDER-$i")).toSet
  private lazy val coupleController = CoupleController(
    AgentPath("AGENT"),
    AgentRunId(JournalId.random()),
    EventId.BeforeFirst,
    ControllerRunId(JournalId.random()))
  private lazy val clientResource = for
    given ActorSystem <- actorSystemResource("CommandWebServerTest", testConfig)
    webServer <- PekkoWebServer.httpResource(findFreeTcpPort(), testConfig, route)
    client <- Resource.fromAutoCloseable(IO(AgentClient(
      Admission(Uri(s"${webServer.localUri}"), userAndPassword = None))))
  yield client

  "Big response" in:
    clientResource.use(_.commandExecute(coupleController))
      .map(response => assert(response == Right(CoupleController.Response(orderIds))))

  private def route =
    decodeRequest/*decompress*/ :
      pathSegments("agent/api/command"):
        new CommandWebService {
          protected def ioRuntime = CommandWebServerTest.this.ioRuntime
          protected def whenShuttingDown = Deferred.unsafe
          protected def config = testConfig

          protected val executeCommand = (command, meta) =>
            IO(
              command match {
                case _: CoupleController => Right(CoupleController.Response(orderIds))
                case _ => fail()
              })

          protected val gateKeeper = new GateKeeper(
            WebServerBinding.localhostHttp(port = 1),
            GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))

          protected val sessionRegister = SessionRegister.forTest[SubagentSession](
            SubagentSession.apply, SessionRegister.TestConfig)
        }.commandRoute

private object CommandWebServerTest:
  private val testConfig =
    config"""
      js7.web.server.auth.public = on
      js7.web.server.shutdown-timeout = 10s
      js7.web.server.shutdown-delay = 500ms
      pekko.http.client.parsing.max-content-length = 100MB
    """.withFallback(AgentConfiguration.DefaultConfig)
