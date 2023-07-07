package js7.data.job

import js7.base.test.OurTestSuite
import js7.data.job.PathExecutable.isAbsolute

final class PathExecutableTest extends OurTestSuite
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
