package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import com.sos.jobscheduler.agent.client.{AgentClient, SimpleAgentClient}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Login, Logout, NoOperation}
import com.sos.jobscheduler.agent.test.AgentTest
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentAddress
import com.sos.jobscheduler.data.session.SessionToken
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class LoginIT extends FreeSpec with AgentTest {

  "Login and Logout" in {
    withClient { client ⇒
      assert(!client.hasSession)

      // Access without Login (Session) is permitted
      client.executeCommand(NoOperation) await 99.s shouldEqual AgentCommand.Accepted
      assert(!client.hasSession)

      // Login and Logout
      val Login.Response(sessionToken) = client.executeCommand(Login) await 99.s
      assert(client.hasSession)
      client.executeCommand(NoOperation) await 99.s shouldEqual AgentCommand.Accepted
      assert(client.hasSession)
      client.executeCommand(Logout) await 99.s shouldEqual AgentCommand.Accepted
      assert(!client.hasSession)

      // Using old SessionToken is Unauthorized
      client.setSessionToken(sessionToken)
      val throwable = intercept[AgentClient.HttpException] {
        client.executeCommand(NoOperation) await 99.s
      }
      throwable.status should (equal(Unauthorized) or equal(Forbidden))
      assert(AgentClient.sessionMayBeLost(throwable))
    }
  }

  "Use of discarded SessionToken is forbidden, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, the client learns about the invalid session.
    withClient { client ⇒
      client.setSessionToken(SessionToken.apply(SecretString("DISCARDED")))
      val throwable = intercept[AgentClient.HttpException] {
        client.executeCommand(NoOperation) await 99.s
      }
      throwable.status should (equal(Unauthorized) or equal(Forbidden))
      assert(AgentClient.sessionMayBeLost(throwable))

      client.clearSession()
      client.executeCommand(Login) await 99.s
      assert(client.hasSession)
      client.executeCommand(Logout) await 99.s
    }
  }

  "Second Login invalidates first Login" in {
    withClient { client ⇒
      val Login.Response(aSessionToken) = client.executeCommand(Login) await 99.s
      assert(client.hasSession)
      client.executeCommand(Login) await 99.s
      assert(client.hasSession)

      withClient { otherClient ⇒
        // Using old SessionToken is Unauthorized
        otherClient.setSessionToken(aSessionToken)
        val throwable = intercept[AgentClient.HttpException] {
          otherClient.executeCommand(NoOperation) await 99.s
        }
        throwable.status should (equal(Unauthorized) or equal(Forbidden))
        assert(AgentClient.sessionMayBeLost(throwable))
      }

      client.executeCommand(Logout) await 99.s shouldEqual AgentCommand.Accepted
      assert(!client.hasSession)
    }
  }

  private def withClient(body: AgentClient ⇒ Unit): Unit =
    autoClosing(SimpleAgentClient(AgentAddress(agent.localUri.toString)))(body)
}
