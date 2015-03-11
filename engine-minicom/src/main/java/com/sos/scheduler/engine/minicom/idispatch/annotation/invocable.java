package com.sos.scheduler.engine.minicom.idispatch.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Annotated method can be dynamically invoked via {@link com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch#invoke}.
 */
@Retention(RUNTIME)
@Target(ElementType.METHOD)
public @interface invocable {}
