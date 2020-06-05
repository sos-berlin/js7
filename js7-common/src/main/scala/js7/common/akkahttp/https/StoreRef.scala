package js7.common.akkahttp.https

import java.net.URL
import js7.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
trait StoreRef {
  def url: URL
  /** Password for file */
  def storePassword: SecretString
}
