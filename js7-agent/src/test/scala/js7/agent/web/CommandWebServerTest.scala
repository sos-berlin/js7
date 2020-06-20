package js7.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.decodeRequest
import cats.effect.Resource
import com.typesafe.config.ConfigFactory
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand._
import js7.agent.web.CommandWebServerTest._
import js7.base.auth.SimpleUser
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.core.command.CommandMeta
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.{EventId, JournalId}
import js7.data.order.OrderId
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServerTest extends AsyncFreeSpec
{
  coupleScribeWithSlf4j()

  private val n = 1_000 //1_000_000
  private lazy val orderIds = (for (i <- 1 to n) yield OrderId(s"A-MEDIUM-LONG-ORDER-$i")).toSet
  private lazy val coupleController = CoupleController(AgentRefPath("/AGENT"), AgentRunId(JournalId.random()), EventId.BeforeFirst)
  private lazy val clientResource = for {
    as <- actorSystemResource("CommandWebServerTest", testConfig)
    webServer <- AkkaWebServer.resourceForHttp(findFreeTcpPort(), route(as), testConfig)(as)
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
          protected def scheduler = Scheduler.global
          protected def isShuttingDown = false
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
            GateKeeper.Configuration.fromConfig(config, SimpleUser.apply))

          protected val sessionRegister = SessionRegister.start[SimpleSession](
            actorSystem, SimpleSession.apply, SessionRegister.TestConfig)
        }.commandRoute
      }
    }
}

private object CommandWebServerTest
{
  private val testConfig =
    ConfigFactory.parseString("""
       js7.web.server.auth.public = on
       js7.web.server.shutdown-timeout = 10s
       akka.http.client.parsing.max-content-length = 100MB"""
    ).withFallback(AgentConfiguration.DefaultConfig)
}
