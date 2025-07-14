package js7.tests.addOrders

import com.typesafe.config.ConfigFactory
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.utils.CatsUtils.*
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.data.workflow.WorkflowPath

private final case class Settings(
  workflowPath: WorkflowPath,
  orderCount: Int,
  admissions: Nel[Admission])
extends BasicConfiguration:
  val config = ConfigFactory.empty

  val name = "TestAddOrders"


private object Settings:
  private val defaultUserAndPassword = UserAndPassword(UserId("demo"), SecretString("demo"))

  def parseArguments(args: Seq[String]): Settings =
    CommandLineArguments.parse(args)(fromCommandLine)

  def fromCommandLine(args: CommandLineArguments): Settings =
    val userAndPassword = args.optionAs[String]("--user=")
      .fold(defaultUserAndPassword): string =>
        string.split(":", 2) match
          case Array(u, password) => UserAndPassword(UserId(u), SecretString(password))
          case Array(u) => UserAndPassword(UserId(u), SecretString.empty)
    Settings(
      args.as[WorkflowPath]("--workflow="),
      args.as[Int]("--count=", 1),
      args.nelAs[Uri]("--controller=").map(Admission(_, Some(userAndPassword))))
