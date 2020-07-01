package js7.common.configutils

import com.typesafe.config.ConfigFactory
import js7.base.generic.GenericString
import js7.common.configutils.Hocon._
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final class HoconTest extends AnyFreeSpec
{
  "hocon string interpolator" - {
    "Simple string" in {
      assert(hocon"""A = "STRING" """ == ConfigFactory.parseMap(Map("A" -> "STRING").asJava))
      assert(hocon"""A = "STRING\"\u007f." """ == ConfigFactory.parseMap(Map("A" -> "STRING\"\u007f.").asJava))
    }

    for (string <- "STRING" :: "STRING\"" :: "STRING\"\u007f." :: Nil) {
      s"Interpolating String: $string" in {
        assert(hocon"""A = "!$string" """ == ConfigFactory.parseMap(Map("A" -> s"!$string").asJava))
      }
    }

    case class MyGenericString(string: String) extends GenericString
    for (string <- MyGenericString("STRING") :: MyGenericString("STRING\")") :: MyGenericString("STRING\"\u007f.") :: Nil) {
      s"Interpolating GenericString: $string" in {
        assert(hocon"""A = "!$string" """ == ConfigFactory.parseMap(Map("A" -> s"!$string").asJava))
      }
    }

    "Interpolating Int value" in {
      val i = 7
      assert(hocon"""A = $i""" == ConfigFactory.parseMap(Map("A" -> 7).asJava))
    }

    "Interpolating Array value" in {
      val array = List(1, 2, 3)
      assert(hocon"""A = $array""" == ConfigFactory.parseMap(Map("A" -> Seq(1, 2, 3).asJava).asJava))
    }
  }
}
