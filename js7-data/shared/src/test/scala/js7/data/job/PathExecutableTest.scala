package js7.data.job

import js7.data.job.PathExecutable.isAbsolute
import org.scalatest.freespec.AnyFreeSpec

final class PathExecutableTest extends AnyFreeSpec
{
  "isAbsolutePaht" in {
    assert(!isAbsolute(""))
    assert(!isAbsolute("X"))
    assert(!isAbsolute("\\"))
    assert(isAbsolute("/"))
    assert(isAbsolute("/A"))
    assert(!isAbsolute("C:"))
    assert(isAbsolute("C:\\"))
    assert(isAbsolute("C:\\A"))
    assert(isAbsolute("C:/"))
    assert(isAbsolute("C:/A"))
  }
}
