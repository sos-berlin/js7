package js7.data.workflow.position

import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class InstructionNrTest extends Test
{
  "Some instances are predefined" in {
    for (i <- 0 to 999) assert(InstructionNr(i) eq InstructionNr(i))
    for (i <- 1000 to 1009) assert(InstructionNr(i) ne InstructionNr(i))
  }
}
