## Java system properties

Some settings cannot be changed in a `*.conf` file but as Java system properties via the `-D` `java` command line option.

The first (bold) value is the default value.

**js7.parser=cats**|fastparse

JS7 v2.5 until v2.5.3 uses the new [cat-parse](https://github.com/typelevel/cats-parse) library. The previously used [fastparse](https://github.com/com-lihaoyi/fastparse) library can be re-enabled with this property.

This setting will no longer be available when the JS7 code has been migrated to Scala 3, presumely with  v2.6.

**js7.strict=false**|true

For testing. With `js7.strict=true`, JS7 tries to be more strict. Some (unusual) operations may fail and JS7 may be (much) slower.

**js7.log.colored=true**|false

Use colored output for some special log lines (mostly for journaled events at the trace level) for better readability.

Use `less -R` to let the color escape sequences take effect.
