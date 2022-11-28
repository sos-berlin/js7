package js7.tests.addOrders

import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.utils.CatsUtils.*
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.data.workflow.WorkflowPath

private final case class Settings(
  workflowPath: WorkflowPath,
  orderCount: Int,
  admissions: Nel[Admission])

private object Settings
{
  private val defaultUserAndPassword = UserAndPassword(UserId("demo"), SecretString("demo"))

  def parseArguments(args: Seq[String]): Settings =
    CommandLineArguments.parse(args) { a =>
      val userAndPassword = a.optionAs[String]("--user=")
        .fold(defaultUserAndPassword) { string =>
          string.split(":", 2) match {
            case Array(u, password) => UserAndPassword(UserId(u), SecretString(password))
            case Array(u) => UserAndPassword(UserId(u), SecretString.empty)
          }
        }
      Settings(
        a.as[WorkflowPath]("--workflow="),
        a.as[Int]("--count=", 1),
        Nel.unsafe(
          a.seqAs[Uri]("--controller=").map(Admission(_, Some(userAndPassword)))))
    }
}
