package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import akka.http.scaladsl.model.headers.{HttpChallenge, HttpChallenges, `WWW-Authenticate`}
import com.sos.jobscheduler.agent.client.{AgentClient, SimpleAgentClient}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.NoOperation
import com.sos.jobscheduler.agent.test.TestAgentProvider
import com.sos.jobscheduler.base.auth.{SessionToken, SimpleUser, UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider.LoginWWWAuthenticate
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Instant.now
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class LoginTest extends FreeSpec with BeforeAndAfterAll with TestAgentProvider {

  override def beforeAll() =
    (configDirectory resolve "private/private.conf").contentString = """jobscheduler.auth.users.USER = "plain:PASSWORD" """

  override def afterAll() = closer closeThen { super.afterAll() }

  private val testUserAndPassword = UserAndPassword(UserId("USER"), SecretString("PASSWORD"))
  private lazy val invalidAuthenticationDelay = agent.injector.instance[GateKeeper.Configuration[SimpleUser]].invalidAuthenticationDelay

  "Login without credentials is rejected with 401 Unauthorized" in {
    withClient { client ⇒
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "JobScheduler Agent")))))
    }
  }

  "Login with invalid credentials is rejected with 403 Unauthorized" in {
    withClient { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(Some(UserId("USER") → SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenge("X-JobScheduler-Login", realm = None)))))
      assert(exception.dataAsString contains "Login: unknown user or invalid password")
      assert((now - t) >= invalidAuthenticationDelay)
    }
  }

  "Login and Logout" in {
    withClient { client ⇒
      assert(!client.hasSession)

      // Access without Login (LoginSession) is permitted
      client.executeCommand(NoOperation) await 99.s shouldEqual AgentCommand.Accepted
      assert(!client.hasSession)

      // Login and Logout
      val sessionToken = client.login(Some(testUserAndPassword)) await 99.s
      assert(client.hasSession)
      client.executeCommand(NoOperation) await 99.s shouldEqual AgentCommand.Accepted
      assert(client.hasSession)
      client.logout() await 99.s shouldEqual Completed
      assert(!client.hasSession)

      // Using old SessionToken is Unauthorized
      client.setSessionToken(sessionToken)
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.executeCommand(NoOperation) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
      assert(AkkaHttpClient.sessionMayBeLost(exception))
    }
  }

  "Use of discarded SessionToken is unauthorized, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, the client learns about the invalid session.
    withClient { client ⇒
      client.setSessionToken(SessionToken(SecretString("DISCARDED")))
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.executeCommand(NoOperation) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
      assert(AkkaHttpClient.sessionMayBeLost(exception))

      client.clearSession()
      client.login(Some(testUserAndPassword)) await 99.s
      assert(client.hasSession)
      client.logout() await 99.s
    }
  }

  "Second Login invalidates first Login" in {
    withClient { client ⇒
      val firstSessionToken = client.login(Some(testUserAndPassword)) await 99.s
      assert(client.hasSession)
      client.login(Some(UserAndPassword(UserId.Anonymous, SecretString("")))) await 99.s
      assert(client.hasSession)

      withClient { otherClient ⇒
        // Using old SessionToken is Unauthorized
        otherClient.setSessionToken(firstSessionToken)
        val exception = intercept[AkkaHttpClient.HttpException] {
          otherClient.executeCommand(NoOperation) await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
        assert(AkkaHttpClient.sessionMayBeLost(exception))
      }

      client.logout() await 99.s shouldEqual Completed
      assert(!client.hasSession)
    }
  }

  private def withClient(body: AgentClient ⇒ Unit): Unit =
    autoClosing(new SimpleAgentClient(agent.localUri)) {
      body
    }
}
