package js7.tests.https

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.http.AkkaHttpClient
import js7.data.cluster.ClusterState
import js7.data.problems.InvalidLoginProblem
import monix.execution.Scheduler.Implicits.traced

final class ControllerClientSideMultiUserHttpsTest extends ControllerHttpsStandardTests
{
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = true
  override protected def controllerHttpsMutual = true
  override protected def provideControllerClientCertificate = true

  override protected def extraDistringuishedNameUserAndPassword =
    Some(UserAndPassword(UserId("EXTRA") -> SecretString("EXTRA-PASSWORD")))
  override protected def useCluster = false

  "Login with second listed UserId is accepted" - {
    addTestsForCredentials(extraDistringuishedNameUserAndPassword)
  }

  "Login without credentials is rejected" in {
    val e = intercept[AkkaHttpClient.HttpException] {
      httpControllerApi.login_(None).await(99.s)
    }
    assert(e.status == Unauthorized && e.problem == Some(InvalidLoginProblem))
  }

  "Login with non-listed UserId is rejected" in {
    val e = intercept[AkkaHttpClient.HttpException] {
      httpControllerApi.login_(Some(otherUserAndPassword)).await(99.s)
    }
    assert(e.status == Unauthorized && e.problem == Some(InvalidLoginProblem))
  }

  "Login with listed UserId but wrong password is rejected" in {
    val e = intercept[AkkaHttpClient.HttpException] {
      httpControllerApi.login_(Some(otherUserAndPassword.copy(password = SecretString("WRONG")))).await(99.s)
    }
    assert(e.status == Unauthorized && e.problem == Some(InvalidLoginProblem))
  }

  "HTTP authentication" - {
    def get(headers: List[HttpHeader]) = {
      import httpControllerApi.implicitSessionToken
      httpControllerApi.httpClient.get[ClusterState](controller.localUri / "controller/api/cluster", headers).await(99.s)
    }

    "Missing authentication is rejected" in {
      val e = intercept[AkkaHttpClient.HttpException] {
        get(Nil)
      }
      assert(e.status == Unauthorized)
    }

    "Valid authentication" in {
      get(Authorization(BasicHttpCredentials("TEST", "TEST-PASSWORD")) ::Nil)
      get(Authorization(BasicHttpCredentials("EXTRA", "EXTRA-PASSWORD")) ::Nil)
    }

    "Wrong authentication" in {
      val e = intercept[AkkaHttpClient.HttpException] {
        get(Authorization(BasicHttpCredentials("TEST", "WRONG-PASSWORD")) ::Nil)
      }
      assert(e.status == Unauthorized)
    }

    "Non-listed UserId is rejected" in {
      val e = intercept[AkkaHttpClient.HttpException] {
        get(Authorization(BasicHttpCredentials(otherUserAndPassword.userId.string, otherUserAndPassword.password.string)) ::Nil)
      }
      assert(e.status == Forbidden &&
        e.problem == Some(Problem("HTTP user does not match UserIds allowed by HTTPS client distinguished name")))
    }
  }

  addTestsForCredentials()
}
