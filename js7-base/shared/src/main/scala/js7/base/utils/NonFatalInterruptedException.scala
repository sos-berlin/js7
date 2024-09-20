package js7.base.utils

/**
 * Because Scala's NonFatal considers InterruptedException as fatal.
 * <p>
 * Uses in case InterruptedException is expected.
 */
final class NonFatalInterruptedException(cause: InterruptedException)
extends RuntimeException(cause)
