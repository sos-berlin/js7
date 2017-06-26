package com.sos.jobscheduler.agent.web.marshal

import com.sos.jobscheduler.agent.data.commandresponses.XmlResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import spray.http.MediaTypes.{`application/json`, `application/xml`, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.ToResponseMarshaller.oneOf
import spray.httpx.marshalling.{Marshaller, ToResponseMarshaller}

/**
 * @author Joacim Zschimmer
 */
object ResponseXmlSupport {

  private val jsonMarshaller: Marshaller[AgentCommand.Response] = AgentCommand.Response.MyJsonFormat

  private val xmlMarshaller: Marshaller[AgentCommand.Response] =
    Marshaller.delegate[AgentCommand.Response, xml.NodeSeq](`application/xml`, `text/xml`) {
      _ match {
        case response: XmlResponse ⇒ <spooler><answer>{response.toXmlElem}</answer></spooler>
        case response ⇒ sys.error(s"${response.getClass} is not a XmlResponse and cannot be marshalled as XML")
      }
    }

  implicit val ResponseMarshaller: ToResponseMarshaller[AgentCommand.Response] =
    oneOf(`application/json`, `application/xml`, `text/xml`)(jsonMarshaller, xmlMarshaller)
}
