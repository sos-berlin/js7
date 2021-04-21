package js7.executor.utils

import js7.executor.configuration.JobExecutorConf.ErrLineLengthMaximum
import org.scalatest.freespec.AnyFreeSpec

final class LastLineKeeperTest extends AnyFreeSpec
{
  "Empty" in {
    val lastLineKeeper = new LastLineKeeper
    assert(lastLineKeeper.lastErrLine == None)
    assert(lastLineKeeper.testLastErrLine == "")
  }

  "Single line" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("LAST\n")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")
  }

  "Two line" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("ONE\n")
    lastLineKeeper.put("LAST\n")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")
  }

  "Two line in same chunk" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("ONE\nLAST\n")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")
  }

  "No line end" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("ONE\nLAST")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST")
  }

  "Line split across multiple chunks" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("ONE")
    lastLineKeeper.put("\nL")
    lastLineKeeper.put("A")
    lastLineKeeper.put("ST")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST")
  }

  "Carriage return" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("LAST\r\n")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\r\n")
  }

  "Split carriage return" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("LAST\r")
    lastLineKeeper.put("\n")
    assert(lastLineKeeper.lastErrLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\r\n")
  }

  "Long line" in {
    val lastLineKeeper = new LastLineKeeper
    lastLineKeeper.put("x" * (ErrLineLengthMaximum + 1) + "\n")
    assert(lastLineKeeper.lastErrLine == Some("x" * (ErrLineLengthMaximum - 3) + "..."))
    assert(lastLineKeeper.testLastErrLine == "x" * (ErrLineLengthMaximum + 1))
  }

  "Long line split accross chunks" in {
    val lastLineKeeper = new LastLineKeeper
    val xxx = "x" * (ErrLineLengthMaximum / 2 - 1)
    val yyy = "y" * (ErrLineLengthMaximum / 2 - 1)
    val zzz = "z" * (ErrLineLengthMaximum / 2 - 1)
    lastLineKeeper.put(xxx)
    lastLineKeeper.put(yyy)
    lastLineKeeper.put(zzz)
    lastLineKeeper.put("---")
    assert(lastLineKeeper.lastErrLine == Some((xxx + yyy).take(ErrLineLengthMaximum - 3) + "..."))
    assert(lastLineKeeper.testLastErrLine == xxx + yyy + "zzz")
  }
}
