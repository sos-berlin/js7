package sos.spooler;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Für PropertiesTest: Die Methode ist der Getter einer Property. */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface SchedulerGetter {}

