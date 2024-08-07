package js7.base.configutils

import cats.Monoid
import cats.syntax.semigroup.*
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import java.nio.file.Paths
import js7.base.configutils.Configs.*
import js7.base.configutils.ConfigsTest.*
import js7.base.generic.GenericString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class ConfigsTest extends OurTestSuite:

  "Config syntax" in:
    assert(TestConfig.getString("string") == "STRING")
    intercept[ConfigException.Missing] { TestConfig.getString("missing") }
    assert(TestConfig.getInt("int") == 42)

  "Convertible syntax" in:
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

  "float as Integer" in:
    assert(TestConfig.getInt("float") == 12)
    intercept[IllegalArgumentException] { TestConfig.as[Int]("float") }

  "boolean" in:
    for ((v, ks) <- List(false -> List("false", "off", "no"),
                        true -> List("true", "on", "yes"));
         k <- ks)
      assert(TestConfig.getBoolean(k) == v)
      assert(TestConfig.as[Boolean](k) == v)

  "checkedPath" in:
    assert(TestConfig.checkedPath("string") (path => Right(TestConfig.getString(path))) == Right("STRING"))
    assert(TestConfig.checkedPath("MISSING") (path => Right(TestConfig.getString(path))) == Left(Problem(s"Missing configuration key 'MISSING'")))

  "ifPath" in:
    assert(TestConfig.ifPath("string") (path => TestConfig.getString(path)) == Some("STRING"))
    assert(TestConfig.ifPath("MISSING") (path => TestConfig.getString(path)) == None)

  "renderValue" in:
    withTemporaryDirectory("ConfigsTest") { dir =>
      val file = dir / "test.conf"
      val hidden = dir / "hidden.conf"
      file := "KEY = VALUE"
      hidden := "SECRET-KEY = SECRET-VALUE"
      val config = parseConfigIfExists(dir / "test.conf", secret = false)
        .withFallback(parseConfigIfExists(dir / "hidden.conf", secret = true))
      assert(renderValue("KEY", config.getValue("KEY")) == s"""KEY="VALUE" ($file: 1)""")
      assert(renderValue("SECRET-KEY", config.getValue("SECRET-KEY")) == "SECRET-KEY=(secret)")
    }

  "config string interpolator" - {
    "Simple string" in:
      assert(config"""A = "STRING" """ == ConfigFactory.parseMap(Map("A" -> "STRING").asJava))
      assert(config"""A = "STRING\"\u007f." """ == ConfigFactory.parseMap(Map("A" -> "STRING\"\u007f.").asJava))

    for string <- Seq("STRING", "STRING\"", "STRING\"\u007f.", "back\\slash") do
      s"Interpolating String: $string" in:
        assert(config"""A = "!$string" """ == ConfigFactory.parseMap(Map("A" -> s"!$string").asJava))

    "Path" in:
      val directory = Paths.get("c:\\windows\\directory")
      val config = config"""directory = "$directory/file" """
      assert(config == ConfigFactory.parseMap(Map("directory" -> "c:\\windows\\directory/file").asJava))

    case class MyGenericString(string: String) extends GenericString
    for string <- MyGenericString("STRING") :: MyGenericString("STRING\")") :: MyGenericString("STRING\"\u007f.") :: Nil do
      s"Interpolating GenericString: $string" in:
        assert(config"""A = "!$string" """ == ConfigFactory.parseMap(Map("A" -> s"!$string").asJava))

    "Interpolating" - {
      "Int" in:
        val i = 7
        assert(config"""A = $i""" == ConfigFactory.parseMap(Map("A" -> 7).asJava))

      "Array" in:
        val array = List(1, 2, 3)
        assert(config"""A = $array""" == ConfigFactory.parseMap(Map("A" -> Seq(1, 2, 3).asJava).asJava))

      "FiniteDuration and Map" in:
        val array = Seq(1.ns, 2.µs, 3.ms, 4.s, 5.minutes, 6.h, 7.days)
        assert(configString"""A = $array""" == "A = [ 1ns, 2microseconds, 3ms, 4s, 5m, 6h, 7d ]")

      "Map" in:
        val array: Map[Any, Any] =
          Map(1 -> 1.s, "file" -> Paths.get("FILE"), "array" -> Seq(1, 2.s))
        assert(configString"""A = $array""" == "A = { 1: 1s, file: FILE, array: [ 1, 2s ] }")
    }
  }

  "Monoid[Config]" in:
    assert(Monoid[Config].empty == ConfigFactory.empty)
    val a = config"""
      X = 1
      Y = 2"""
    val b = config"""
      X = 10
      Z = 30"""
    val combined = Monoid[Config].combine(a, b)
    assert(combined == b.withFallback(a))
    assert(combined.getInt("X") == 10)
    assert(combined == (a |+| b))


object ConfigsTest:

  private val TestConfig = config"""
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
    emptySeq = []
    """
