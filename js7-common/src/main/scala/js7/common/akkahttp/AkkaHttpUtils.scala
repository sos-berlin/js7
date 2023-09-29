package js7.common.akkahttp

import akka.http.scaladsl.model.HttpEntity.ChunkStreamPart
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.stream.scaladsl.Source
import akka.util.ByteString

object AkkaHttpUtils:
  /** Try to avoid lazy object initialization deadlock in `HttpEntity`.
    *
    * On deadlock the stack looks like this:
    * <pre>
    * Thread js7-controller-web-controller-api-log-LogRouteTest-akka.actor.default-dispatcher-7 Object.wait()
    * akka.http.javadsl.model.HttpEntity$ChunkStreamPart.&lt;clinit>(HttpEntity.java:325)
    * akka.http.scaladsl.model.HttpEntity$Chunked$$anonfun$fromData$1.applyOrElse(HttpEntity.scala:562)
    * akka.http.scaladsl.model.HttpEntity$Chunked$$anonfun$fromData$1.applyOrElse(HttpEntity.scala:561)
    * </pre>
    */
  def avoidLazyObjectInitializationDeadlock(): Unit =
    HttpEntity.Chunk("X")
    HttpEntity.Chunked(ContentTypes.`application/json`, Source.single(ChunkStreamPart("X")))
    HttpEntity.Chunked.fromData(ContentTypes.`application/json`, Source.single(ByteString("X")))
    HttpEntity.ChunkStreamPart("X")
    HttpEntity.LastChunk
