package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.client.{AgentClient, SimpleAgentClient}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.jobscheduler.agent.data.commands.{Login, Logout, NoOperation}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.session.SessionToken
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
