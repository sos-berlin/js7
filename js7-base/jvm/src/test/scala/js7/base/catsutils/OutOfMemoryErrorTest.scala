package js7.base.catsutils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Tests.isIntelliJIdea

final class OutOfMemoryErrorTest extends OurAsyncTestSuite:

  "JVM dies on OutOfMemoryError?" in:
    if !isIntelliJIdea then pending
    IO:
      throw new OutOfMemoryError("TEST OutOfMemoryError")
      // TODO How to let die the JVM ?
