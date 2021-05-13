package js7.executor.forwindows

import js7.base.io.process.KeyLogin
import js7.base.problem.Checked

private[executor] final case class WindowsLogon(
  credential: WindowsProcessCredential,
  withUserProfile: Boolean)
{
  def userName = credential.userName
}

private[executor] object WindowsLogon
{
  def fromKeyLogin(keyLogin: KeyLogin): Checked[WindowsLogon] =
    for (cred <- WindowsProcessCredential.byKey(keyLogin.credentialKey)) yield
      WindowsLogon(cred, keyLogin.withUserProfile)
}
