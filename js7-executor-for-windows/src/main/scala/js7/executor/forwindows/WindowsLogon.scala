package js7.executor.forwindows

import js7.base.io.process.KeyLogin
import js7.base.problem.Checked

private final case class WindowsLogon(
  credentials: WindowsProcessCredentials,
  withUserProfile: Boolean)
{
  def user = credentials.user
}

private object WindowsLogon
{
  def fromKeyLogin(keyLogin: KeyLogin): Checked[WindowsLogon] =
    for (cred <- WindowsProcessCredentials.byKey(keyLogin.credentialKey)) yield
      WindowsLogon(cred, keyLogin.withUserProfile)
}
