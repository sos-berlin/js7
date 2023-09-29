package js7.data.workflow.position

import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class InstructionNrTest extends OurTestSuite
{
  "Some instances are predefined" in {
    for i <- 0 to 999 do assert(InstructionNr(i) eq InstructionNr(i))
    for i <- 1000 to 1009 do assert(InstructionNr(i) ne InstructionNr(i))
  }
}
