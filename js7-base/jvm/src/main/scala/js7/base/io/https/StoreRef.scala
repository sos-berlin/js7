package js7.base.io.https

import java.net.URL
import java.nio.file.{FileSystems, Path}
import js7.base.generic.SecretString
import js7.base.utils.ScalaUtils.syntax.RichBoolean

/**
  * @author Joacim Zschimmer
  */
trait StoreRef {
  def url: URL
  /** Password for file */
  def storePassword: SecretString

  def toFile: Option[Path] = {
    val uri = url.toURI
    (uri.getScheme == "file") ?
      // Path.of(uri) <-- requires Java 11
      FileSystems.getDefault.provider.getPath(uri)
  }
}
