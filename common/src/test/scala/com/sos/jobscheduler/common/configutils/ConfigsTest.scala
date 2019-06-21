package com.sos.jobscheduler.common.configutils

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.configutils.ConfigsTest._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.typesafe.config.{ConfigException, ConfigFactory}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ConfigsTest extends FreeSpec
{
  "Config syntax" in {
    assert(TestConfig.getString("string") == "STRING")
    intercept[ConfigException.Missing] { TestConfig.getString("missing") }
    assert(TestConfig.getInt("int") == 42)
  }

  "Convertible syntax" in {
    assert(TestConfig.as[String]("string") == "STRING")
    assert(TestConfig.optionAs[String]("string") == Some("STRING"))
    assert(TestConfig.optionAs[String]("missing") == None)
    assert(TestConfig.as[Int]("int") == 42)
    assert(TestConfig.seqAs[String]("seq") == List("1", "2", "3"))
    assert(TestConfig.seqAs[Int]("seq") == List(1, 2, 3))
    assert(TestConfig.seqAs[Int]("emptySeq") == Vector.empty)
    intercept[ConfigException.Missing] { TestConfig.seqAs[Int]("missing") }
    assert(TestConfig.seqAs[Int]("missing", Nil) == Vector.empty)
    assert(TestConfig.seqAs[Int]("missing", List(7)) == List(7))
    intercept[ConfigException.WrongType] { TestConfig.seqAs[Int]("int") }
  }

  "float as Integer" in {
    assert(TestConfig.getInt("float") == 12)
    intercept[IllegalArgumentException] { TestConfig.as[Int]("float") }
  }

  "boolean" in {
    for ((v, ks) <- List(false -> List("false", "off", "no"),
                        true -> List("true", "on", "yes"));
         k <- ks) {
      assert(TestConfig.getBoolean(k) == v)
      assert(TestConfig.as[Boolean](k) == v)
    }
  }

  "checkedPath" in {
    assert(TestConfig.checkedPath("string") (path => Valid(TestConfig.getString(path))) == Valid("STRING"))
    assert(TestConfig.checkedPath("MISSING") (path => Valid(TestConfig.getString(path))) == Invalid(Problem(s"Missing configuration key 'MISSING'")))
  }

  "ifPath" in {
    assert(TestConfig.ifPath("string") (path => TestConfig.getString(path)) == Some("STRING"))
    assert(TestConfig.ifPath("MISSING") (path => TestConfig.getString(path)) == None)
  }

  "renderConfig" in {
    withTemporaryDirectory("ConfigsTest") { dir =>
      val file = dir / "test.conf"
      val hidden = dir / "hidden.conf"
      file := "KEY = VALUE"
      hidden := "SECRET-KEY = SECRET-VALUE"
      val config = parseConfigIfExists(dir / "test.conf", secret = false)
        .withFallback(parseConfigIfExists(dir / "hidden.conf", secret = true))
      assert(renderConfig(config) == s"KEY=VALUE ($file: 1)" :: "SECRET-KEY=(secret)" :: Nil)
    }
  }
}

object ConfigsTest
{
  private val TestConfig = ConfigFactory.parseString("""
    string = STRING
    int = 42
    float = 12.9
    false = false
    off = off
    no = no
    true = true
    on = on
    yes = yes
    seq = [1, 2, 3]
    emptySeq = []""".stripMargin)
}
