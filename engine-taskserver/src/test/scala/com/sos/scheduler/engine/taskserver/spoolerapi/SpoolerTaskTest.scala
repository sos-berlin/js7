package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.{AnnotatedInvocable, IDispatch}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SpoolerTaskTest extends FreeSpec {

  private object TestSpoolerTask extends SpoolerTask with IDispatch.Empty with AnnotatedInvocable {
    // Mockito cannot handle IUnknown's final hashCode or final equals()
    def setErrorCodeAndText(code: MessageCode, text: String) = throw new UnsupportedOperationException
    def paramsXml = <sos.spooler.variable_set><variable name="T" value="t"/></sos.spooler.variable_set>.toString()
    def paramsXml_=(o: String) = throw new UnsupportedOperationException
    def orderParamsXml = <sos.spooler.variable_set><variable name="O" value="o"/></sos.spooler.variable_set>.toString()
    def orderParamsXml_=(o: String) = throw new UnsupportedOperationException
  }

  "parameterMap" in {
    assert(TestSpoolerTask.parameterMap == Map("T" → "t"))
  }

  "orderParameterMap" in {
    assert(TestSpoolerTask.orderParameterMap == Map("O" → "o"))
  }
}
