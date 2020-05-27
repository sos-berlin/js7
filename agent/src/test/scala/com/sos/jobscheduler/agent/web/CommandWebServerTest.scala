package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives.decodeRequest
import cats.effect.Resource
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.web.CommandWebServerTest._
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.Akkas.actorSystemResource
import com.sos.jobscheduler.common.log.ScribeUtils.coupleScribeWithSlf4j
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.{EventId, JournalId}
import com.sos.jobscheduler.data.order.OrderId
import com.typesafe.config.ConfigFactory
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
  private lazy val coupleMaster = CoupleMaster(AgentRunId(JournalId.random()), EventId.BeforeFirst)
  private lazy val clientResource = for {
    as <- actorSystemResource("CommandWebServerTest", testConfig)
    webServer <- AkkaWebServer.resourceForHttp(findFreeTcpPort(), route(as), testConfig)(as)
    client <- Resource.fromAutoCloseable(Task(AgentClient(Uri(s"${webServer.localUri}"), userAndPassword = None)(as)))
  } yield client

  "Big response" in {
    clientResource.use(_.commandExecute(coupleMaster))
      .map(response => assert(response == Right(CoupleMaster.Response(orderIds))))
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
                case _: CoupleMaster => Right(CoupleMaster.Response(orderIds))
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

private object CommandWebServerTest {
  private val testConfig =
    ConfigFactory.parseString("""
       jobscheduler.webserver.auth.public = on
       jobscheduler.webserver.shutdown-timeout = 10s
       akka.http.client.parsing.max-content-length = 100MB"""
    ).withFallback(AgentConfiguration.DefaultConfig)
}
