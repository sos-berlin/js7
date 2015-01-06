package com.sos.scheduler.engine.data.base;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

/** @author Joacim Zschimmer */
public final class IsStringSerializer extends JsonSerializer<IsString> {
    public static final IsStringSerializer singleton = new IsStringSerializer();

    private IsStringSerializer() {}

    @Override public Class<IsString> handledType() {
        return IsString.class;
    }

    @Override
    public void serialize(IsString o, JsonGenerator g, SerializerProvider p) throws IOException {
        g.writeString(o.string());
    }
}
