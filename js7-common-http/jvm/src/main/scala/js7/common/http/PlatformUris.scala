package js7.common.http

import org.apache.pekko.http.scaladsl.model.Uri

/**
  * @author Joacim Zschimmer
  */
trait PlatformUris extends Uris {

  final def encodePath(segments: String*): String =
    segments map encodeSegment mkString "/"

  final def encodeSegment(string: String): String =
    Uri.Path./(string).toString.stripPrefix("/")

  final def encodeQuery(kvs: (String, String)*): String =
    encodeQuery(kvs)

  final def encodeQuery(kvs: Iterable[(String, String)]): String =
    if (kvs.isEmpty)
      ""
    else
      "?" + Uri.Query(kvs.toSeq*).toString
}
