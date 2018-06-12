package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.Validated.Valid
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.command.CommandMeta
import com.sos.jobscheduler.master.configuration.MasterConfiguration.DefaultConfig
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends FreeSpec with ScalatestRouteTest with ApiRootRoute {

  protected val masterId = MasterId("TEST-MASTER")
  protected implicit def scheduler = Scheduler.global
  protected val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(DefaultConfig, SimpleUser.apply),
    new TimerService(Some(1.s)),
    isHttps = false)
  protected val sessionRegister = SessionRegister.start[LoginSession.Simple](system, LoginSession.Simple.apply, akkaAskTimeout = 60.seconds)

  protected def executeCommand(command: MasterCommand, meta: CommandMeta) =
    command match {
      case MasterCommand.Terminate ⇒ Task.pure(Valid(MasterCommand.Response.Accepted))
      case _ ⇒ fail()
    }

  protected def orderCount = Task.pure(7)

  private def route: Route =
    pathSegments("api") {
      apiRootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[MasterOverview]
      assert(overview.id == masterId)
      assert(overview.version == BuildInfo.buildVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.orderCount == 7)
    }
  }

  "POST /api" in {
    Post("/api", MasterCommand.Terminate: MasterCommand) ~> route → check {
      val response = responseAs[MasterCommand.Response]
      assert(response == MasterCommand.Response.Accepted)
    }
  }
}
