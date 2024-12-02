package js7.base.problem

import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.Tests.isTest
import scala.collection.immutable.Map.Map2
import js7.base.log.Logger

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
    def apply(typ: => String, key: Any)
      (using filename: sourcecode.FileName, line: sourcecode.Line)
    : UnknownKeyProblem =
      val problem = new UnknownKeyProblem(typ, key)
      logger.debug(s"❓${filename.value}:${line.value} $problem")
      problem


  final case class DuplicateKey(typ: String, key: Any) extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "type", typ,
      "key", key.toString)


  case object InvalidSessionTokenProblem extends Problem.ArgumentlessCoded


  case object ShuttingDownProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503  // Service Unavailable


  case object WebServiceStillNotAvailableProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 503 // Service Unavailable
