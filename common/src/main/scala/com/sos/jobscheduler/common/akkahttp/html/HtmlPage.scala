package com.sos.jobscheduler.common.akkahttp.html

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.model.{ContentType, HttpEntity}
import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Logger
import scala.language.implicitConversions
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

  implicit val marshaller: ToEntityMarshaller[HtmlPage] =
    Marshaller.withOpenCharset(`text/html`) { (htmlPage, charset) ⇒
      try {
        val sb = new StringBuilder(10000)
        sb.append("<!DOCTYPE html>")
        htmlPage.wholePage.writeTo(sb)
        HttpEntity(ContentType(`text/html`, charset), ByteString.fromString(sb.toString, charset.nioCharset))
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
