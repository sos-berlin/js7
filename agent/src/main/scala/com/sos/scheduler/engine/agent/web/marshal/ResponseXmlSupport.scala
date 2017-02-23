package com.sos.scheduler.engine.agent.web.marshal

import com.sos.scheduler.engine.agent.data.commandresponses.{Response, XmlResponse}
import spray.http.MediaTypes.{`application/json`, `application/xml`, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.ToResponseMarshaller.oneOf
import spray.httpx.marshalling.{Marshaller, ToResponseMarshaller}

/**
 * @author Joacim Zschimmer
 */
object ResponseXmlSupport {

  private val jsonMarshaller: Marshaller[Response] = Response.MyJsonFormat

  private val xmlMarshaller: Marshaller[Response] =
    Marshaller.delegate[Response, xml.NodeSeq](`application/xml`, `text/xml`) {
      _ match {
        case response: XmlResponse ⇒ <spooler><answer>{response.toXmlElem}</answer></spooler>
        case response ⇒ sys.error(s"${response.getClass} is not a XmlResponse and cannot be marshalled as XML")
      }
    }

  implicit val ResponseMarshaller: ToResponseMarshaller[Response] =
    oneOf(`application/json`, `application/xml`, `text/xml`)(jsonMarshaller, xmlMarshaller)
}
