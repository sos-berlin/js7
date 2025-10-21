package js7.base.problem

import js7.base.log.Logger
import js7.base.scalasource.ScalaSourceLocation
import scala.collection.immutable.Map.Map2

object Problems:

  private val logger = Logger[this.type ]

  final case class InvalidNameProblem(typeName: String, name: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "type", typeName,
      "name", name)


  final class UnknownKeyProblem(_typ: => String, val key: Any)
  extends Problem.Coded:
    lazy val typ: String = _typ

    def arguments: Map[String, String] =
      Map2(
        "type", typ,
        "key", key.toString)

  object UnknownKeyProblem:
    def apply(typ: => String, key: Any)(using loc: ScalaSourceLocation)
    : UnknownKeyProblem =
      val problem = new UnknownKeyProblem(typ, key)
      logger.debug(s"❓$loc $problem")
      problem


  final case class DuplicateKey(typ: String, key: Any) extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "type", typ,
      "key", key.toString)


  case object InvalidSessionTokenProblem extends Problem.ArgumentlessCoded


  type ShuttingDownProblem = ShuttingDownProblem.type
  case object ShuttingDownProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable


  case object WebServiceStillNotAvailableProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable
