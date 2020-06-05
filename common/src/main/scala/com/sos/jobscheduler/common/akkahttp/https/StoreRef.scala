package js7.common.akkahttp.https

import js7.base.generic.SecretString
import java.net.URL

/**
  * @author Joacim Zschimmer
  */
trait StoreRef {
  def url: URL
  /** Password for file */
  def storePassword: SecretString
}
