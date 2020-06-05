package js7.common.http

import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
trait PlatformUris extends Uris {

  final def encodePath(segments: String*): String =
    segments map encodeSegment mkString "/"

  final def encodeSegment(string: String): String =
    js.URIUtils.encodeURIComponent(string)

  final def encodeQuery(kvs: (String, String)*): String =
    encodeQuery(kvs)

  final def encodeQuery(kvs: Iterable[(String, String)]): String =
    if (kvs.isEmpty)
      ""
    else
      "?" + (for ((k, v) <- kvs) yield encodeSegment(k) + "=" + encodeSegment(v)).mkString("&")
}
