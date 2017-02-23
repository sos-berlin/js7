package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.common.auth.User
import com.sos.jobscheduler.common.soslicense.LicenseKeyBunch
import com.sos.jobscheduler.data.session.SessionToken
import java.net.InetAddress

/**
 * @author Joacim Zschimmer
 */
final case class CommandMeta(
  user: User = User.Anonymous,
  clientIpOption: Option[InetAddress] = None,
  sessionTokenOption: Option[SessionToken] = None,
  licenseKeyBunch: LicenseKeyBunch = LicenseKeyBunch())

object CommandMeta {
  private val Empty = new CommandMeta

  def apply(): CommandMeta = Empty
}
