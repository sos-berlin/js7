package com.sos.scheduler.engine.agent.web

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.agent.configuration.Akkas._
import com.sos.scheduler.engine.agent.data.FileOrderSourceContent
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.JsonUtils.jsonQuote
import com.sos.scheduler.engine.data.base.IsString
import java.net.InetAddress
import java.nio.file.Files.{createTempDirectory, setLastModifiedTime}
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Paths}
import java.time.{ZoneId, ZonedDateTime}
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.Future
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.InternalServerError
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling._
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServiceTest extends FreeSpec with BeforeAndAfterAll with ScalatestRouteTest with AgentWebService with HasCloser {

  implicit lazy val actorRefFactory = newActorSystem("TEST")(closer)

  override def afterAll(): Unit = {
    close()
    super.afterAll()
  }

  "Command" in {
    postCommand("test") ~> check {
      responseAs[String] shouldEqual "<ok/>"
    }
  }

  "Command throwing Exception" in {
    postCommand("error") ~> check {
      assert(status == InternalServerError)
    }
  }

  private def postCommand(command: String): RouteResult =
    Post("/jobscheduler/engine/command", command) ~>
      addHeader("Remote-Address", "0.0.0.0") ~>   // For this IP-less test only. Client's IP is normally set by configuration spray.can.remote-address-header
      route

  protected def executeCommand(clientIP: InetAddress, command: String) = Future.successful[xml.Elem](
    command match {
      case "test" ⇒ <ok/>
      case _ ⇒ throw new Exception("TEST EXCEPTION")
    })

  "agent/fileOrderSource/newFiles" in {
    val dir = createTempDirectory("agent-")

    val aTime = ZonedDateTime.of(2015, 5, 1, 12, 0, 0, 0, ZoneId.of("UTC")).toInstant
    val xTime = aTime + 2.s
    val cTime = aTime + 4.s
    onClose { Files.delete(dir) }
    val expectedResult = FileOrderSourceContent(List(
      FileOrderSourceContent.Entry((dir / "a").toString, aTime.toEpochMilli),
      FileOrderSourceContent.Entry((dir / "x").toString, xTime.toEpochMilli),
      FileOrderSourceContent.Entry((dir / "c").toString, cTime.toEpochMilli)))
    for (entry ← expectedResult.files) {
      val path = Paths.get(entry.path)
      touch(path)
      setLastModifiedTime(path, FileTime.fromMillis(entry.lastModifiedTime))
      onClose { Files.delete(path) }
    }
    val knownFile = dir / "known"
    touch(knownFile)
    onClose { Files.delete(knownFile) }

    implicit val JsObjectMarshaller: Marshaller[JsObject] = Marshaller.of[JsObject](`application/json`) { (value, contentType, ctx) ⇒
      ctx.marshalTo(HttpEntity(contentType, value.compactPrint))
    }

    Post(Uri("/jobscheduler/agent/fileOrderSource/newFiles"), JsObject(
        "directory" → JsString(dir.toString),
        "regex" → JsString(".*"),
        "durationMillis" → JsNumber(Long.MaxValue),
        "knownFiles" → JsArray(JsString(knownFile.toString)))) ~>
      Accept(MediaTypes.`application/json`) ~>
      route ~> check
    {
      assert(responseAs[FileOrderSourceContent] == expectedResult)
      assert(responseAs[String].parseJson ==
        s"""{
          "files": [
            { "path": ${jsonQuote(dir / "a")}, "lastModifiedTime": ${aTime.toEpochMilli} },
            { "path": ${jsonQuote(dir / "x")}, "lastModifiedTime": ${xTime.toEpochMilli} },
            { "path": ${jsonQuote(dir / "c")}, "lastModifiedTime": ${cTime.toEpochMilli} }
          ]
        }""".parseJson)
    }
  }
}

private object AgentWebServiceTest {
  final case class Json(string: String) extends IsString
}
