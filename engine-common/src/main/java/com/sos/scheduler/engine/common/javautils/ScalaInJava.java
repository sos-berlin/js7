package com.sos.scheduler.engine.common.javautils;

import java.util.Optional;
import java.util.concurrent.Future;

/**
 * Public functions for use of Scala in Java.
 *
 * @author Joacim Zschimmer
 */
public class ScalaInJava {
    private ScalaInJava() {}

    /**
     * @return A copy of the argument list as an immutable.Set, ignoring duplicates
     */
    @SafeVarargs
    public static <A> scala.collection.immutable.Set<A> toScalaSet(A... o) {
        @SuppressWarnings("varargs")
        scala.collection.immutable.Set<A> result = ScalaInJavaHelper$.MODULE$.toScalaSet(o);
        return result;
    }

    public static <A> Future<A> asJavaFuture(scala.concurrent.Future<A> scalaFuture) {
        return new ScalaInJavaFuture<>(scalaFuture);
    }

    public static <A> Optional<A> toJavaOptional(scala.Option<A> scalaOption) {
        return scalaOption.isDefined() ? Optional.of(scalaOption.get()) : Optional.empty();
    }
}
