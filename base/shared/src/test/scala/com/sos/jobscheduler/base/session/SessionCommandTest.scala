package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword, UserId}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SessionCommandTest extends AnyFreeSpec {

  "JSON" - {
    "Login" in {
      testJson[SessionCommand](SessionCommand.Login(Some(UserAndPassword(UserId("USER"), SecretString("PASSWORD")))),
        json"""{
          "TYPE": "Login",
          "userAndPassword": {
            "userId": "USER",
            "password": "PASSWORD"
          }
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
      testJson[SessionCommand.Response](SessionCommand.Login.LoggedIn(SessionToken(SecretString("SESSION-TOKEN"))),
        json"""{
          "TYPE": "LoggedIn",
          "sessionToken": "SESSION-TOKEN"
        }""")
    }
  }
}
