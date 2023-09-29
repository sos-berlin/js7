package js7.proxy.javaapi.eventbus

trait EventSubscription
extends AutoCloseable:
  // No checked exceptions
  def close(): Unit
