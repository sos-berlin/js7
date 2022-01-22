package js7.base.session

import js7.base.auth.{SessionToken, UserAndPassword, UserId}
import js7.base.circeutils.CirceUtils._
import js7.base.generic.SecretString
import js7.base.version.Version
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SessionCommandTest extends AnyFreeSpec {

  "JSON" - {
    "Login" in {
      testJson[SessionCommand](SessionCommand.Login(
        Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD"))),
        Some(Version("0.0.0-TEST"))),
        json"""{
          "TYPE": "Login",
          "userAndPassword": {
            "userId": "USER",
            "password": "PASSWORD"
          },
          "js7Version": "0.0.0-TEST"
        }""")
    }

    "Logout" in {
      testJson[SessionCommand](SessionCommand.Logout(SessionToken(SecretString("SESSION-TOKEN"))),
        json"""{
          "TYPE": "Logout",
          "sessionToken": "SESSION-TOKEN"
        }""")
    }

    "LoggedIn" in {
      testJson[SessionCommand.Response](SessionCommand.Login.LoggedIn(
        SessionToken(SecretString("SESSION-TOKEN")),
        Version("0.0.0-TEST")),
        json"""{
          "TYPE": "LoggedIn",
          "sessionToken": "SESSION-TOKEN",
          "js7Version": "0.0.0-TEST"
        }""")
    }
  }
}
