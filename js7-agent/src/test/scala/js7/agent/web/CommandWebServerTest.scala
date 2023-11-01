package js7.agent.web

import cats.effect.Resource
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.*
import js7.agent.web.CommandWebServerTest.*
import js7.agent.web.common.AgentSession
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.*
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.test.OurAsyncTestSuite
import js7.base.web.Uri
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.pekkoutils.Pekkos.actorSystemResource
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.core.command.CommandMeta
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.event.{EventId, JournalId}
import js7.data.order.OrderId
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.traced
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.decodeRequest
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServerTest extends OurAsyncTestSuite
{
  coupleScribeWithSlf4j()

  private val n = 1_000 //1_000_000
  private lazy val orderIds = (for (i <- 1 to n) yield OrderId(s"A-MEDIUM-LONG-ORDER-$i")).toSet
  private lazy val coupleController = CoupleController(AgentPath("AGENT"), AgentRunId(JournalId.random()), EventId.BeforeFirst)
  private lazy val clientResource = for {
    as <- actorSystemResource("CommandWebServerTest", testConfig)
    webServer <- PekkoWebServer.resourceForHttp(findFreeTcpPort(), route(as), testConfig)(as)
    client <- Resource.fromAutoCloseable(Task(AgentClient(Uri(s"${webServer.localUri}"), userAndPassword = None)(as)))
  } yield client

  "Big response" in {
    clientResource.use(_.commandExecute(coupleController))
      .map(response => assert(response == Right(CoupleController.Response(orderIds))))
      .runToFuture
  }

  private def route(implicit actorSystem: ActorSystem) =
    decodeRequest/*decompress*/ {
      pathSegments("agent/api/command") {
        new CommandWebService {
          protected def scheduler = Scheduler.traced
          protected def whenShuttingDown = Future.never
          protected def config = testConfig
          protected def commandOverview = throw new NotImplementedError
          protected def commandDetailed = throw new NotImplementedError

          protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
            Task(
              command match {
                case _: CoupleController => Right(CoupleController.Response(orderIds))
                case _ => fail()
              })

          protected val gateKeeper = new GateKeeper(
            WebServerBinding.Http,
            GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))

          protected val sessionRegister = SessionRegister.start[AgentSession](
            actorSystem, AgentSession.apply, SessionRegister.TestConfig)
        }.commandRoute
      }
    }
}

private object CommandWebServerTest
{
  private val testConfig =
    config"""
      js7.web.server.auth.public = on
      js7.web.server.shutdown-timeout = 10s
      pekko.http.client.parsing.max-content-length = 100MB
    """.withFallback(AgentConfiguration.DefaultConfig)
}
