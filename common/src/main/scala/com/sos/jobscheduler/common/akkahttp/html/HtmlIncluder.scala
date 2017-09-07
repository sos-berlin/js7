package com.sos.jobscheduler.common.akkahttp.html

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkahttp.html.HtmlIncluder._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.utils.JavaResource
import java.math.BigInteger
import java.net.URL
import java.security.MessageDigest
import scala.collection.immutable
import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
  * @author Joacim Zschimmer
  */
final class HtmlIncluder(webServiceContext: WebServiceContext) {

  import webServiceContext.{staticFilesResource, toWebjarUri}

  def webjarsToHtml(webjar: Webjar): immutable.Seq[Frag] =
    (webjar.cssPaths map toWebjarUri map toCssLinkHtml) ++
      (webjar.javascriptPaths map toWebjarUri map toScriptHtml) ++
      (for (initialize ← webjar.initialize) yield script(`type` := "text/javascript")(s"jQuery(function() { $initialize })"))

  def cssHtml(webjar: Webjar): Frag =
    webjar.cssPaths map toWebjarUri map toCssLinkHtml

  def javascriptHtml(webjar: Webjar): Frag =
    (webjar.javascriptPaths map toWebjarUri map toScriptHtml) ++
      (for (initialize ← webjar.initialize) yield script(`type` := "text/javascript")(s"jQuery(function() { $initialize })"))

  def toVersionedUriPath(resource: JavaResource): String = {
    require(resource.path startsWith staticFilesResource.path)
    val path = resource.path.stripPrefix(staticFilesResource.path)
    val hash = uriToSha224(resource.url)
    s"api/frontend/$path?SHA-224=$hash"
  }
}

object HtmlIncluder {

  def toCssLinkHtml(uri: Uri): TypedTag[String] =
    link(rel := "stylesheet", `type` := "text/css", href := uri.toString)

  def toScriptHtml(uri: Uri): TypedTag[String] =
    script(`type` := "text/javascript", src := uri.toString)

  //def toAsyncScriptHtml(uri: Uri) = script(`type` := "text/javascript", src := uri.toString, "async".emptyAttr)

  private[html] def uriToSha224(url: URL): String = {
    val messageDigest = MessageDigest.getInstance("SHA-224")
    autoClosing(url.openStream()) { in ⇒
      val buffer = new Array[Byte](4096)
      var end = false
      while (!end) {
        val len = in.read(buffer)
        if (len > 1) {
          messageDigest.update(buffer, 0, len)
        } else {
          end = true
        }
      }
    }
    toHexString(messageDigest.digest)
  }

  private def toHexString(bytes: Array[Byte]): String =
    new BigInteger(+1, bytes) formatted s"%0${bytes.length}x"
}
