package js7.common.akkahttp.web.auth

import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.directives.Credentials
import js7.base.auth.{HashedPassword, SimpleUser, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.test.OurTestSuite
import js7.common.akkahttp.web.auth.OurMemoizingAuthenticatorTest.*
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OurMemoizingAuthenticatorTest extends OurTestSuite
{
  private val accessCounter = mutable.Map[UserId, Int]()
  private lazy val authenticator = new OurMemoizingAuthenticator[SimpleUser](userId => {
    accessCounter.update(userId, accessCounter.getOrElse(userId, 0) + 1)
    userId match {
      case AUser.id => Some(AUser)
      case BUser.id => Some(BUser)
      case UserId.Anonymous => Some(SimpleUser.TestAnonymous)
      case _ => None
    }
  })

  "tests" in {
    assert(authenticator(Credentials(Some(BasicHttpCredentials("A", "abc")))) == Some(AUser))
    assert(authenticator(Credentials(Some(BasicHttpCredentials("A", "WRONG")))) == None)
    assert(authenticator(Credentials(Some(BasicHttpCredentials("B", "123")))) == Some(BUser))
  }

  "Anonymous is rejected" in {
    assert(authenticator(Credentials(Some(BasicHttpCredentials(UserId.Anonymous.string, "")))) == None)
  }

  "Missing credential: return predefined Anonymous if its password is empty" in {
    assert(authenticator(Credentials(None)) == Some(SimpleUser.TestAnonymous))
  }

  "Missing credential rejected if user account Anonymous has a non-empty password" in {
    val authenticator = new OurMemoizingAuthenticator[SimpleUser]({
      case UserId.Anonymous => Some(SimpleUser(UserId.Anonymous, HashedPassword(SecretString("NON-EMPTY"), identity)))
      case o => throw new MatchError(o)
    })
    assert(authenticator(Credentials(None)) == None)
  }

  "authenticate" in {
    assert(authenticator.authenticate(UserAndPassword(UserId("x") -> SecretString("xxx"))) == None)
    assert(authenticator.authenticate(UserAndPassword(UserId("A") -> SecretString("cba"))) == None)
    assert(authenticator.authenticate(UserAndPassword(UserId("A") -> SecretString("abc"))) == Some(AUser))
  }

  "cached" in {
    assert(accessCounter == Map(
      AUser.id -> 1,
      BUser.id -> 1,
      UserId.Anonymous -> 1,
      UserId("x") -> 1))  // Unknown users are memoized, too !!!
  }
}

object OurMemoizingAuthenticatorTest {
  private val AUser = SimpleUser(UserId("A"), HashedPassword(SecretString("cba"), _.reverse))
  private val BUser = SimpleUser(UserId("B"), HashedPassword(SecretString("321"), _.reverse))
}
