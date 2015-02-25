package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderState
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class OrderCommandTest extends FreeSpec {
  "OrderCommand" in {
    val orderKey = JobChainPath("/JOBCHAIN") orderKey "ID"
    OrderCommand(orderKey).xmlElem shouldEqual <order job_chain="/JOBCHAIN" id="ID"/>
    OrderCommand(orderKey, suspended = Some(true)).xmlElem shouldEqual <order job_chain="/JOBCHAIN" id="ID" suspended="true"/>
    OrderCommand(orderKey, state = Some(OrderState("STATE"))).xmlElem shouldEqual <order job_chain="/JOBCHAIN" id="ID" state="STATE"/>
    OrderCommand(orderKey, title= Some("TITLE")).xmlElem shouldEqual <order job_chain="/JOBCHAIN" id="ID" title="TITLE"/>
    OrderCommand(orderKey, parameters = Map("a" -> "1", "b" -> "2")).xmlElem shouldEqual
      <order job_chain="/JOBCHAIN" id="ID"><params><param name="a" value="1"/><param name="b" value="2"/></params></order>
  }
}
