package js7.tests.controller.proxy

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.proxy.javaapi.JCredentials
import js7.tests.testenv.ControllerClusterForScalaTest

private[proxy] trait ClusterProxyTest extends ControllerClusterForScalaTest
{
  override protected def primaryControllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
        permissions = [ UpdateRepo ]
      }
    }
    """

  override protected def backupControllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-BACKUP"
        permissions = [ UpdateRepo ]
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
}
