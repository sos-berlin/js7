package js7.base.problem

object Problems
{
  final case class InvalidNameProblem(typeName: String, name: String) extends Problem.Coded {
    def arguments = Map(
      "type" -> typeName,
      "name" -> name
    )
  }

  final class UnknownKeyProblem(_typ: => String, val key: Any) extends Problem.Coded {
    lazy val typ = _typ

    def arguments = Map(
      "type" -> typ,
      "key" -> key.toString)
  }
  object UnknownKeyProblem {
    def apply(typ: => String, key: Any) = new UnknownKeyProblem(typ, key)
  }

  final case class DuplicateKey(typ: String, key: Any) extends Problem.Coded {
    def arguments = Map(
      "type" -> typ,
      "key" -> key.toString)
  }

  case object InvalidSessionTokenProblem extends Problem.ArgumentlessCoded

  case object ShuttingDownProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // Service Unavailable
  }
}
