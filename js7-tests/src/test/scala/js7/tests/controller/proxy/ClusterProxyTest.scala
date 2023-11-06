package js7.tests.controller.proxy

import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.generic.SecretString
import js7.base.problem.Checked.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.pekkoutils.ProvideActorSystem
import js7.controller.client.PekkoHttpControllerApi
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.data_for_java.auth.JCredentials
import js7.proxy.ControllerApi
import js7.proxy.configuration.ProxyConfs
import js7.tests.controller.proxy.ClusterProxyTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.{BeforeAndAfterAll, TestSuite}

trait ClusterProxyTest extends BeforeAndAfterAll with ControllerClusterForScalaTest with ProvideActorSystem
{
  this: TestSuite =>

  protected val items = Seq(workflow)
  protected def config = config"""
    js7.web.client.failure-delays = [ 0.1s ]
    """.withFallback(ProxyConfs.defaultConfig)

  protected val admissions = Nel.of(
    Admission(Uri(s"http://127.0.0.1:$primaryControllerPort"), Some(primaryUserAndPassword)),
    Admission(Uri(s"http://127.0.0.1:$backupControllerPort"), Some(backupUserAndPassword)))

  protected lazy val controllerApi = new ControllerApi(
    admissions.zipWithIndex
      .traverse { case (a, i) =>
        PekkoHttpControllerApi.resource(
          Admission(a.uri, a.userAndPassword),
          name = s"${getClass.simpleScalaName}-Controller-$i")(
          actorSystem)
      },
    ProxyConfs.fromConfig(config))

  override def afterAll() = {
    close()
    controllerApi.stop.runAsyncAndForget
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
    """
      define workflow {
        execute executable="TEST.cmd", agent="AGENT", processLimit=10;
      }"""
  ).orThrow
}
