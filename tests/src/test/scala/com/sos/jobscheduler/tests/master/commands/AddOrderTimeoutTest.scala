package com.sos.jobscheduler.tests.master.commands

import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.tests.master.commands.AddOrderTimeoutTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class AddOrderTimeoutTest extends FreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = Nil
  protected val fileBased = workflow :: Nil
  override protected val masterConfig = ConfigFactory.parseString("""
    jobscheduler.webserver.auth.public = on
    jobscheduler.akka.ask-timeout = 1s
    jobscheduler.TEST-ONLY.add-order-delay = 10s
    """)

  "AddOrder timeout is returned as 403 Service Unavailable" in {
    val status = intercept[HttpException] {
      master.httpApi.addOrder(FreshOrder(OrderId("ORDER"), workflow.path)).await(99.s)
    }.status
    assert(status == ServiceUnavailable)
  }
}

object AddOrderTimeoutTest
{
  private val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "1","""
      define workflow {
        execute executable="/SCRIPT1.cmd", agent="/AGENT";
      }"""
  ).orThrow
}
