package com.sos.scheduler.engine.common.maven

import com.sos.scheduler.engine.common.maven.MavenPropertiesTest._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class MavenPropertiesTest extends FreeSpec {

  private val m = MavenProperties.EngineCommonMavenProperties

  "Maven?" in {
    logger.info(s"isMaven=$isMaven")
  }

  "groupId" in {
    assert(m.groupId == "com.sos-berlin.jobscheduler.engine")
  }

  "artifactId" in {
    assert(m.artifactId startsWith "common_"/*+scala binaryVersion*/)
  }

  "version" in {
    assert(m.version contains '.')
    assert(!(m.version contains ' '))
  }

  "versionCommitHash" in {
    assert(m.versionCommitHash forall "0123456789abcdefABCDEF".toSet)
    assert(m.versionCommitHash.nonEmpty == isMaven)
  }

  "buildDateTime" in {
    assert(m.buildDateTime.toInstant isAfter (now() - 3.h))   // Could fail, when artifact has not been recently completely compiled
    assert(m.buildDateTime.toInstant isBefore (now() + 1.s))
  }

  "buildVersion" in {
    logger.info(s"buildVersion = ${m.buildVersion}")
    if (m.version endsWith "-SNAPSHOT") {
      assert(m.buildVersion startsWith s"${m.version} ")
    } else {
      assert(m.buildVersion == m.version)
    }
  }
}

object MavenPropertiesTest {
  private val logger = Logger(getClass)
  private val isMaven = (new Exception).getStackTrace exists { _.getClassName startsWith "org.apache.maven." }
}
