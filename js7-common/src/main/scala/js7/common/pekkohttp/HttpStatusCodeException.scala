package js7.common.pekkohttp

import js7.base.problem.Problem
import org.apache.pekko.http.scaladsl.model.StatusCode

/**
  * @author Joacim Zschimmer
  */
final class HttpStatusCodeException(val statusCode: StatusCode, val problem: Problem)
extends RuntimeException:
  override def getMessage = s"${statusCode.intValue} ${statusCode.value} $problem".trim
