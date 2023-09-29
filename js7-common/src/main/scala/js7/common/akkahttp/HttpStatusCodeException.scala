package js7.common.akkahttp

import akka.http.scaladsl.model.StatusCode
import js7.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final class HttpStatusCodeException(val statusCode: StatusCode, val problem: Problem)
extends RuntimeException:
  override def getMessage = s"${statusCode.intValue} ${statusCode.value} $problem".trim
