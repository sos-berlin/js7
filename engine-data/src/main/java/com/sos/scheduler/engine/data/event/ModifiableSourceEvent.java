package com.sos.scheduler.engine.data.event;

/** Markiert ein {@link com.sos.scheduler.engine.data.event.Event}, dass dessen com.sos.scheduler.engine.eventbus.EventSource
 * unter bestimmten Bedingungen
 * von einen @com.sos.scheduler.engine.eventbus.HotEventHandler verändert werden darf. */
public interface ModifiableSourceEvent {}
