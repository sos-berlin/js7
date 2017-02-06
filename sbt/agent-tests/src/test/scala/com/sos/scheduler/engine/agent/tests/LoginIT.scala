package com.sos.scheduler.engine.agent.tests

import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.client.{AgentClient, SimpleAgentClient}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.scheduler.engine.agent.data.commands.{Login, Logout, NoOperation}
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.session.SessionToken
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import spray.http.StatusCodes.{Forbidden, Unauthorized}
import spray.httpx.UnsuccessfulResponseException

/**
  * @author Joacim Zschimmer
  */
final class LoginIT extends FreeSpec with BeforeAndAfterAll {

  private lazy val agent = new Agent(AgentConfiguration.forTest())

  override protected def beforeAll() = {
    agent.start() await 5.s
    super.beforeAll()
  }

  override protected def afterAll() = {
    agent.close()
    super.afterAll()
  }

  "Login and Logout" in {
    withClient { client ⇒
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

  "Use of discarded SessionToken is forbidden, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, the client learns about the invalid session.
    withClient { client ⇒
      client.setSessionToken(SessionToken.apply(SecretString("DISCARDED")))
      val throwable = intercept[UnsuccessfulResponseException] {
        client.executeCommand(NoOperation) await 99.s
      }
      throwable.response.status should (equal(Unauthorized) or equal(Forbidden))
      assert(AgentClient.sessionIsPossiblyLost(throwable))

      client.clearSession()
      client.executeCommand(Login) await 99.s
      assert(client.hasSession)
      client.executeCommand(Logout) await 99.s
    }
  }

  "Second Login invalidates first Login" in {
    withClient { client ⇒
      val LoginResponse(aSessionToken) = client.executeCommand(Login) await 99.s
      assert(client.hasSession)
      client.executeCommand(Login) await 99.s
      assert(client.hasSession)

      withClient { otherClient ⇒
        // Using old SessionToken is Unauthorized
        otherClient.setSessionToken(aSessionToken)
        val throwable = intercept[UnsuccessfulResponseException] {
          otherClient.executeCommand(NoOperation) await 99.s
        }
        throwable.response.status should (equal(Unauthorized) or equal(Forbidden))
        assert(AgentClient.sessionIsPossiblyLost(throwable))
      }

      client.executeCommand(Logout) await 99.s shouldEqual EmptyResponse
      assert(!client.hasSession)
    }
  }

  private def withClient(body: AgentClient ⇒ Unit): Unit =
    autoClosing(SimpleAgentClient(agent.localUri.toString))(body)
}
