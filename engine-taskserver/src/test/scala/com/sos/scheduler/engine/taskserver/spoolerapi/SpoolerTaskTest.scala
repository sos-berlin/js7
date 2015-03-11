package com.sos.scheduler.engine.taskserver.spoolerapi

import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SpoolerTaskTest extends FreeSpec {

  private abstract class AbstractSpoolerTask extends SpoolerTask  // Mockito doesn't like traits

  "parameterMap" in {
    val spoolerTask = mock[AbstractSpoolerTask]
    when(spoolerTask.paramsXml) thenReturn <sos.spooler.variable_set><variable name="T" value="t"/></sos.spooler.variable_set>.toString()
    assert(spoolerTask.parameterMap == Map("T" → "t"))
  }

  "orderParameterMap" in {
    val spoolerTask = mock[AbstractSpoolerTask]
    when(spoolerTask.orderParamsXml) thenReturn <sos.spooler.variable_set><variable name="O" value="o"/></sos.spooler.variable_set>.toString()
    assert(spoolerTask.orderParameterMap == Map("O" → "o"))
  }
}
