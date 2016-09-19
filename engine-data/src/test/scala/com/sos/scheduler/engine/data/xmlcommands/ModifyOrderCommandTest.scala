package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ModifyOrderCommandTest extends FreeSpec {

  "ModifyOrderCommand" in {
    val orderKey = JobChainPath("/JOBCHAIN") orderKey "ID"
    ModifyOrderCommand(orderKey).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID"/>
    ModifyOrderCommand(orderKey, suspended = Some(true)).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" suspended="true"/>
    ModifyOrderCommand(orderKey, nodeId = Some(NodeId("STATE"))).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" state="STATE"/>
    ModifyOrderCommand(orderKey, title= Some("TITLE")).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" title="TITLE"/>
//    ModifyOrderCommand(orderKey, parameters = Map("a" -> "1", "b" -> "2")).xmlElem shouldEqual
//      <modify_order job_chain="/JOBCHAIN" order="ID"><params><param name="a" value="1"/><param name="b" value="2"/></params></order>
    ModifyOrderCommand(orderKey, at = Some(ModifyOrderCommand.NowAt)).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" at="now"/>
    ModifyOrderCommand(orderKey, action = Some(ModifyOrderCommand.Action.reset)).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" action="reset"/>
  }

  "startNow" in {
    val orderKey = JobChainPath("/JOBCHAIN") orderKey "ID"
    ModifyOrderCommand.startNow(orderKey).xmlElem shouldEqual <modify_order job_chain="/JOBCHAIN" order="ID" at="now"/>
  }
}
