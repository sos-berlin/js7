package js7.master.web.master.api.graphql

import akka.http.scaladsl.model.ContentTypes.`text/html(UTF-8)`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.circe.parser.{parse => parseJson}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils._
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.html.HtmlDirectives.htmlPreferred
import js7.common.utils.JavaResource
import js7.core.filebased.FileBasedApi
import js7.data.filebased.FileBased
import js7.data.order.{Order, OrderId}
import js7.master.OrderApi
import js7.master.web.common.MasterRouteProvider
import js7.master.web.master.api.graphql.GraphqlRoute._
import monix.execution.Scheduler
import sangria.ast.Document
import sangria.execution.Executor
import sangria.marshalling.circe._
import sangria.parser.QueryParser
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
trait GraphqlRoute extends MasterRouteProvider
{
  protected def orderApi: OrderApi
  protected def fileBasedApi: FileBasedApi

  private implicit def implicitScheduler: Scheduler = scheduler

  private val context = new QueryContext {
    def scheduler = GraphqlRoute.this.scheduler

    def order(orderId: OrderId) =
      orderApi.order(orderId)

    def orders(selector: QueryContext.OrderFilter) = {
      val idMatches: Order[Order.State] => Boolean =
        selector.idPattern match {
          case None => _ => true
          case Some(pattern) => o => pattern.matcher(o.id.string).matches
        }
      val workflowMatches: Order[Order.State] => Boolean =
        selector.workflowPath match {
          case None => _ => true
          case Some(workflowPath) => _.workflowId.path == workflowPath
        }

      def matches(order: Order[Order.State]) = workflowMatches(order) && idMatches(order)

      orderApi.orders.map(_.map(_.view.filter(matches).take(selector.limit).toSeq))
    }

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      fileBasedApi.idTo[A](id)
  }

  final val graphqlRoute: Route =
    pathEnd {
      authorizedUser(ValidUserPermission) { _ =>
        standardGraphqlRoute
      }
    } ~
    path("schema") {
      completeWith(StringMarshaller)(_(MasterGraphqlSchema.schema.renderPretty))
    }

  private def standardGraphqlRoute: Route =
    htmlPreferred {
      getFromResource(GraphiqlResource.path, `text/html(UTF-8)`, GraphiqlResource.classLoader)
    } ~
    get {
      parameters(("query", "operationName".?, "variables".?)) { (queryString, operationName, variables) =>
        parseJson(variables getOrElse "{}") match {
          case Left(t) => completeWithFailure(t)
          case Right(json) => executeGraphql(queryString, operationName, json)
        }
      }
    } ~
    post {
      parameters(("query".?, "operationName".?, "variables".?)) { (queryParam, operationNameParam, variablesParam) =>
        entity(as[JsonObject]) { body =>
          // GraphiQL GUI may send both query parameter and JSON content. For GraphiQL, POST content has precedence
          val queryString = body("query") flatMap (_.asString) orElse queryParam
          val operationName = body("operationName") flatMap (_.asString) orElse operationNameParam
          val checkedVariables = body("variables")
            .map(json => if (json.isNull) EmptyObject else json)
            .map(Right.apply)
            .orElse(variablesParam map (_.parseJsonChecked))
            .getOrElse(Right(EmptyObject))

          queryString match {
            case None => completeWithFailure("Missing query")
            case Some(q) =>
              checkedVariables match {
                case Left(problem) => completeWithFailure(problem.toString)
                case Right(variables) =>
                  executeGraphql(q, operationName, variables)
              }
          }
        }
      }
    }

  private def executeGraphql(queryString: String, operationName: Option[String], variables: Json): Route = {
    QueryParser.parse(queryString).toEither match {
      case Left(throwable) => completeWithFailure(throwable)
      case Right(query) =>
        onComplete(executeGraphql(query, operationName, variables)) {
          case Failure(t) => completeWithFailure(t)
          case Success(json) => complete(json)
        }
    }
  }

  private val EmptyObject = Json.obj()

  private def executeGraphql(query: Document, operationName: Option[String], variables: Json): Future[Json] =
    Executor.execute(MasterGraphqlSchema.schema, query, context,
      operationName = operationName,
      variables = variables)

  private def completeWithFailure(throwable: Throwable): Route =
    throwable match {
      case parsingFailure: io.circe.ParsingFailure =>
        completeWithFailure(parsingFailure.toStringWithCauses)

      case syntaxError: sangria.parser.SyntaxError =>
        complete(BadRequest ->
          Json.obj("errors" -> Json.arr(
            Json.obj(
              "message" -> Option(syntaxError.getMessage).getOrElse("").asJson,
              "locations" -> Json.arr(Json.obj(
                "line" -> syntaxError.originalError.position.line.asJson,
                "column" -> syntaxError.originalError.position.column.asJson))))))

      case throwable @ (_: sangria.execution.ExecutionError | _: sangria.execution.QueryAnalysisError) =>
        completeWithFailure(Option(throwable.getMessage) getOrElse throwable.toStringWithCauses)

      case _ => throw throwable
    }

  private def completeWithFailure(message: String) =
    complete(BadRequest -> errorToJson(message))

  private def errorToJson(message: String): Json =
    Json.obj("errors" -> Json.arr(Json.obj("message" -> message.asJson)))
}

object GraphqlRoute
{
  private val GraphiqlResource = JavaResource("js7/master/web/master/api/graphql/graphiql.html")
}
