package js7.base

import js7.base.test.OurTestSuite

final class BuildInfoTest extends OurTestSuite:

  "BuildInfo" in:
    assert(BuildInfo.version.matches("""[0-9]+\.[0-9]+\.[0-9]+.*"""))
    assert(BuildInfo.longVersion.matches("""[0-9]+\.[0-9]+\.[0-9]+.*"""))
    assert(BuildInfo.prettyVersion.matches("""[0-9]+\.[0-9]+\.[0-9]+.*"""))
    assert(BuildInfo.buildId.length >= 5)
    //assert(BuildInfo.commitId.length == 40)
    //assert(BuildInfo.javaVersion >= 17)
    //assert(BuildInfo.javaRuntimeVersion.head.isDigit)
    assert(BuildInfo.catsEffectVersion.matches("""[0-9]+\.[0-9]+\.[0-9]+.*"""))
