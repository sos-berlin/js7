package com.sos.jobscheduler.data.workflow.position

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InstructionNrTest extends AnyFreeSpec
{
  "Some instances are predefined" in {
    for (i <- 0 to 999) assert(InstructionNr(i) eq InstructionNr(i))
    for (i <- 1000 to 1009) assert(InstructionNr(i) ne InstructionNr(i))
  }
}
