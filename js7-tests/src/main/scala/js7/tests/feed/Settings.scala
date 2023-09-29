package js7.tests.feed

import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.utils.CatsUtils.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments

final case class Settings(admissions: Nel[Admission])

object Settings:
  private val defaultUserAndPassword = Option(UserAndPassword(UserId("demo"), SecretString("demo")))

  def parseArguments(args: Seq[String]): Settings =
    CommandLineArguments.parse(args) { a =>
      val userAndPassword = a.optionAs[String]("--user=")
        .fold(defaultUserAndPassword) { string =>
          string.nonEmpty ?
            (string.split(":", 2) match {
              case Array(u, password) => UserAndPassword(UserId(u), SecretString(password))
              case Array(u) => UserAndPassword(UserId(u), SecretString.empty)
            })
        }
      Settings(
        Nel.unsafe(a.seqAs[Uri]("--controller=").map(Admission(_, userAndPassword))))
    }
