package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword, UserId}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SessionCommandTest extends FreeSpec {

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
      testJson[SessionCommand](SessionCommand.Logout,
        json"""{
          "TYPE": "Logout"
        }""")
    }

    "LoggedIn" in {
      testJson[SessionCommand.Response](SessionCommand.Login.Response(SessionToken(SecretString("SESSION-TOKEN"))),
        json"""{
          "TYPE": "LoggedIn",
          "sessionToken": "SESSION-TOKEN"
        }""")
    }
  }
}
