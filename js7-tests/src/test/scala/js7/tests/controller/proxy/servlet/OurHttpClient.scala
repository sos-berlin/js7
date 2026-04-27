package js7.tests.controller.proxy.servlet

import cats.effect.{IO, Resource, ResourceIO}
import java.net.http.HttpClient.newHttpClient
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import js7.base.system.Java17Polyfill.shutdownNow

private final class OurHttpClient(val javaHttpClient: HttpClient):

  def send[A](
    request: HttpRequest,
    responseBodyHandler: HttpResponse.BodyHandler[A])
  : IO[HttpResponse[A]] =
    IO.fromCompletableFuture:
      IO:
        javaHttpClient.sendAsync(request, responseBodyHandler)


private object OurHttpClient:

  def resource: ResourceIO[OurHttpClient] =
    Resource.make(
      acquire = IO(OurHttpClient(newHttpClient())))(
      release = myHttpClient =>
        IO.blocking:
          myHttpClient.javaHttpClient.shutdownNow()) // Only implemented in Java 21 !!!
