package js7.tests.controller.proxy

import cats.effect.Resource
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.akkautils.ProvideActorSystem
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.data_for_java.auth.JCredentials
import js7.proxy.configuration.ProxyConfs
import js7.tests.controller.proxy.ClusterProxyTest._
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.eval.Task
import org.scalatest.{BeforeAndAfterAll, TestSuite}

trait ClusterProxyTest extends BeforeAndAfterAll with ControllerClusterForScalaTest with ProvideActorSystem
{
  this: TestSuite =>

  protected val items = Seq(workflow)
  protected def config = ProxyConfs.defaultConfig

  protected val admissions = List(
    Admission(Uri(s"http://127.0.0.1:$primaryControllerPort"), Some(primaryUserAndPassword)),
    Admission(Uri(s"http://127.0.0.1:$backupControllerPort"), Some(backupUserAndPassword)))

  protected lazy val apiResources: Seq[Resource[Task, HttpControllerApi]] =
    for ((a, i) <- admissions.zipWithIndex)
      yield AkkaHttpControllerApi.resource(
        a.uri,
        a.userAndPassword,
        name = s"${getClass.simpleScalaName}-Controller-$i")(
        actorSystem)

  override def afterAll() = {
    close()
    super.afterAll()
  }

  override protected def primaryControllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateItem ]
      }
    }
    """

  override protected def backupControllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-BACKUP"
        permissions = [ UpdateItem ]
      }
    }
    """
}

private[proxy] object ClusterProxyTest
{
  private[proxy] val primaryUserAndPassword = UserAndPassword(UserId("Proxy") -> SecretString("PROXYS-PASSWORD-FOR-PRIMARY"))
  private[proxy] val primaryCredentials = JCredentials.JUserAndPassword(primaryUserAndPassword)
  private[proxy] val backupUserAndPassword = UserAndPassword(UserId("Proxy"), SecretString("PROXYS-PASSWORD-FOR-BACKUP"))
  private[proxy] val backupCredentials = UserAndPassword(UserId("Proxy"), SecretString("PROXYS-PASSWORD-FOR-BACKUP"))

  private[proxy] val workflow = WorkflowParser.parse(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    s"""
      define workflow {
        execute executable="TEST.cmd", agent="AGENT", taskLimit=10;
      }"""
  ).orThrow
}
