package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.xmlcommands.ModifyJobCommand.Cmd.{Stop, Unstop}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ModifyJobCommandTest extends FreeSpec {

  "ModifyJobCommand" in {
    ModifyJobCommand(JobPath("/a")).xmlElem shouldEqual <modify_job job="/a"/>
    ModifyJobCommand(JobPath("/a"), cmd = Some(Stop)).xmlElem shouldEqual <modify_job job="/a" cmd="stop"/>
    ModifyJobCommand(JobPath("/a"), cmd = Some(Unstop)).xmlElem shouldEqual <modify_job job="/a" cmd="unstop"/>
  }
}
