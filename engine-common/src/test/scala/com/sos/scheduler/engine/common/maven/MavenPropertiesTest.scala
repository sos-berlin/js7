package com.sos.scheduler.engine.common.maven

import com.sos.scheduler.engine.common.maven.MavenPropertiesTest._
import com.sos.scheduler.engine.common.scalautil.Logger
import org.joda.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class MavenPropertiesTest extends FreeSpec {

  private val mavenProperties = MavenProperties.EngineCommonMavenProperties

  "Maven?" in {
    logger.info(s"isMaven=$isMaven")
  }
  
  "groupId" in {
    assert(mavenProperties.groupId == "com.sos-berlin.jobscheduler.engine")
  }

  "artifactId" in {
    assert(mavenProperties.artifactId == "engine-common")
  }

  "version" in {
    assert(mavenProperties.version contains ".")
  }

  "versionCommitHash" in {
    if (isMaven) assert(mavenProperties.versionCommitHash forall "0123456789abcdefABCDEF".toSet)
    else assert(mavenProperties.versionCommitHash == "(unknown-commit)")
  }

  "buildDateTime" in {
    assert(mavenProperties.buildDateTime isAfter (now() minus 3600000))
    assert(mavenProperties.buildDateTime isBefore (now() plus 1000))
  }
}

object MavenPropertiesTest {
  private val logger = Logger(getClass)
  private val isMaven = (new Exception).getStackTrace exists { _.getClassName startsWith "org.apache.maven." }
}
