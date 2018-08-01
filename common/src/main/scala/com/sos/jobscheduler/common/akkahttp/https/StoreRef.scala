package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.base.generic.SecretString
import java.net.URL

/**
  * @author Joacim Zschimmer
  */
trait StoreRef {
  def url: URL
  /** Password for file */
  def storePassword: SecretString
}
