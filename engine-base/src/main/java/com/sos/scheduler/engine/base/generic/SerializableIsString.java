package com.sos.scheduler.engine.base.generic;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/** Wegen Scala 2.10.3 und Jackson 2.3.0: scala: error while loading IsString, class file 'C:\sos\scheduler\out\production\data\com\sos\scheduler\engine\data\base\IsString.class' is broken
 (class java.lang.RuntimeException/Scala class file does not contain Scala annotation) */
@JsonSerialize(using = IsStringSerializer.class)
interface SerializableIsString {
    String string();
}
