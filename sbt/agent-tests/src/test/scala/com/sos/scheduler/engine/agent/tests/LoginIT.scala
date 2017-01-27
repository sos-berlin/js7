package com.sos.scheduler.engine.agent.tests

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.client.AgentClient
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.scheduler.engine.agent.data.commands.{Login, Logout, NoOperation}
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import spray.http.StatusCodes.{Forbidden, Unauthorized}
import spray.httpx.UnsuccessfulResponseException

/**
  * @author Joacim Zschimmer
  */
final class LoginIT extends FreeSpec with BeforeAndAfterAll {

  "Login and Logout" in {
    val agentConf = AgentConfiguration.forTest()
    autoClosing(new Agent(agentConf)) { agent ⇒
      agent.start() await 5.s
      withCloser { implicit closer ⇒
        implicit val actorSystem = newActorSystem(getClass.getSimpleName) withCloser { _.shutdown() }
        val client = AgentClient(agent.localUri.toString)
        assert(!client.hasSession)

        // Access without Login (Session) is permitted
        client.executeCommand(NoOperation) await 99.s shouldEqual EmptyResponse
        assert(!client.hasSession)

        // Login and Logout
        val LoginResponse(sessionToken) = client.executeCommand(Login) await 99.s
        assert(client.hasSession)
        client.executeCommand(NoOperation) await 99.s shouldEqual EmptyResponse
        assert(client.hasSession)
        client.executeCommand(Logout) await 99.s shouldEqual EmptyResponse
        assert(!client.hasSession)

        // Using old SessionToken is Unauthorized
        client.setSessionToken(sessionToken)
        val throwable = intercept[UnsuccessfulResponseException] {
          client.executeCommand(NoOperation) await 99.s
        }
        throwable.response.status should (equal(Unauthorized) or equal(Forbidden))
        assert(AgentClient.sessionIsPossiblyLost(throwable))
      }
    }
  }
}
