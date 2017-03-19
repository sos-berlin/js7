package com.sos.jobscheduler.common.sprayutils.html

import com.sos.jobscheduler.common.scalautil.Logger
import scala.language.implicitConversions
import spray.http.HttpEntity
import spray.http.MediaTypes.`text/html`
import spray.httpx.marshalling.Marshaller
import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
  * @author Joacim Zschimmer
  */
trait HtmlPage {
  def wholePage: TypedTag[String]
}

object HtmlPage {
  val EmptyFrag: Frag = SeqFrag[Frag](Nil)
  private val logger = Logger(getClass)

  implicit val marshaller: Marshaller[HtmlPage] =
    Marshaller.of[HtmlPage](`text/html`) { (htmlPage, contentType, ctx) ⇒
      try {
        val sb = new StringBuilder(10000)
        sb.append("<!DOCTYPE html>")
        htmlPage.wholePage.writeTo(sb)
        ctx.marshalTo(HttpEntity(contentType, sb.toString))
      } catch {
        case e: OutOfMemoryError ⇒
          logger.error(e.toString)
          throw new RuntimeException(e.toString, e)  // To avoid termination of Akka
      }
    }

  def joinHtml(glue: Frag)(elements: Iterable[Frag]): Frag =
    elements reduce { (a, b) ⇒ seqFrag(a, glue, b) }

  def seqFrag(frags: Frag*): Frag = SeqFrag(frags)
}
