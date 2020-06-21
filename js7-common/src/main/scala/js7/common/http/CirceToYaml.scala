package js7.common.http

import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json, Printer}
import js7.base.circeutils.AnyJavaJsonCodecs.jsonToJava
import js7.base.circeutils.AnyJsonCodecs.anyToJson
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SideEffect.ImplicitSideEffect
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object CirceToYaml
{
  private val LineLength = 120
  private implicit val printer = Printer.noSpaces.copy(dropNullValues = true/*Suppress None*/)

  implicit final class ToYamlString[A](private val underlying: A) extends AnyVal {
    /**
      * Converts this with spray-json to YAML.
      */
    def toYamlString(implicit encoder: Encoder[A]): String =
      yaml.dump(jsonToYaml(underlying.asJson))

    /**
      * Converts this with spray-json to YAML using flow styles.
      * This should return a one-line string.
      */
    def toFlowYamlString(implicit encoder: Encoder[A]): String =
      CirceToYaml.toYamlString(underlying.asJson, FlowStyle.FLOW).trim
  }

  def toYamlString(json: Json, flowStyle: FlowStyle): String =
    yaml.dumpAs(yaml.load(printer.print(json)), Tag.MAP, flowStyle)

  def jsonToYaml(json: Json) = jsonToJava(json)

  def yamlToJson(yamlString: String): Checked[Json] =
    try Right(anyToJson(yaml.load(yamlString)))
    catch {
      case e: org.yaml.snakeyaml.parser.ParserException =>
        YamlProblem(e.toStringWithCauses, e.getProblemMark.getLine, e.getProblemMark.getColumn)

      case NonFatal(t) =>
        Left(Problem.pure(t))
    }

  private val dumperOptions =
    new DumperOptions sideEffect { o =>
      o.setDefaultFlowStyle(FlowStyle.BLOCK)
      o.setWidth(LineLength)
    }

  def yaml: Yaml =
    new Yaml(dumperOptions) // Mutable

  final case class YamlProblem(yamlMessage: String, line: Int, column: Int)
  extends Problem.Lazy("YAML error: " + yamlMessage)
}
