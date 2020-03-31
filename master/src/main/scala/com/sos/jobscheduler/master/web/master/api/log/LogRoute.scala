package com.sos.jobscheduler.master.web.master.api.log

import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.ParameterDirectives._
import akka.http.scaladsl.server.directives.PathDirectives.pathEnd
import akka.http.scaladsl.server.directives.RouteDirectives.{complete, reject}
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers.logAkkaStreamErrorToWebLog
import com.sos.jobscheduler.common.files.GrowingFileObservable
import com.sos.jobscheduler.common.http.AkkaHttpUtils.AkkaByteVector
import com.sos.jobscheduler.common.http.StreamingSupport._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.log.LogRoute._
import java.nio.file.Path
import monix.execution.Scheduler

trait LogRoute extends MasterRouteProvider
{
  protected def scheduler: Scheduler
  protected def currentLogFile: Path

  implicit private def implicitScheduler = scheduler

  lazy val logRoute: Route =
    authorizedUser(ValidUserPermission)(_ =>
      pathEnd(
        parameter("snapshot".as[Boolean] ? false) { snapshot =>
          streamFile(currentLogFile, endless = !snapshot)
          //Fails if files grows while read (Content-Length mismatch?): getFromFile(currentLogFile, contentType)
        }))

  private def streamFile(file: Path, endless: Boolean) =
    if (file.isFile && file.canRead)
      complete(
        HttpEntity(
          contentType,
          logAkkaStreamErrorToWebLog(
            new GrowingFileObservable(file, endless ? pollDelay)
              .map(_.toByteString)
              .toAkkaSource)))
    else
      reject
}

object LogRoute
{
  private val contentType = `text/plain(UTF-8)`
  private val pollDelay = 100.ms
}
