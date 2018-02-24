package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.base.circeutils.AnyJavaJsonCodecs.jsonToJava
import com.sos.jobscheduler.base.circeutils.AnyJsonCodecs.anyToJson
import com.sos.jobscheduler.base.utils.SideEffect.ImplicitSideEffect
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json, Printer}
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.{DumperOptions, Yaml}

/**
  * @author Joacim Zschimmer
  */
object CirceToYaml {
  private implicit val printer = Printer.noSpaces.copy(dropNullValues = true/*Suppress None*/)

  implicit class ToYamlString[A](val delegate: A) extends AnyVal {
    /**
      * Converts this with spray-json to YAML.
      */
    def toYamlString(implicit encoder: Encoder[A]): String = CirceToYaml.toYamlString(delegate.asJson)

    /**
      * Converts this with spray-json to YAML using flow styles.
      * This should return a one-line string.
      */
    def toFlowYamlString(implicit encoder: Encoder[A]): String = CirceToYaml.toYamlString(delegate.asJson, FlowStyle.FLOW).trim
  }

  def toYamlString(v: Json): String =
    yaml.dump(jsonToYaml(v))

  def toYamlString(json: Json, flowStyle: FlowStyle): String =
    yaml.dumpAs(yaml.load(printer.pretty(json)), Tag.MAP, flowStyle)

  def jsonToYaml(json: Json) = jsonToJava(json)

  def yamlToJson(yamlString: String): Json =
    anyToJson(yaml.load(yamlString))

  def yaml: Yaml = {
    val options = new DumperOptions sideEffect { o ⇒
      o.setDefaultFlowStyle(FlowStyle.BLOCK)
      o.setWidth(100)
    }
    new Yaml(options)
  }
}
