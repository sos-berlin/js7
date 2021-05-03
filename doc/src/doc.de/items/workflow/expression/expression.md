# Ausdrücke

JS7 hat einen Kalkül für logische und andere Ausdrücke,
die in der If-Anweisung und zum Setzen von Variablen verwendbar ist.

Beispiele
* `true`
* `1`
* `"STRING"`
* `$VARIABLE`
* `$returnCode <= 3`
* `$NUMBER + 1`


Zum Beispiel das Feld `predicate` in der If-Anweisung.
```json
{
  "TYPE": "If",
  "predicate": "$returnCode >= 3",
  ...
}
```

```json
{
  "TYPE": "Execute.Anonymous",
  "job": {
    "agentPath": "AGENT",
    "executable": {
      "TYPE": "PathExecutable",
      "file": "script.sh",
      "env": {
        "CONSTANT": "\"STRING-CONSTANT\"",
        "VAR": "$VAR",
        "NUMBER": "7",
        "RC": "$returnCode + 1"
      }
    }
  }
}
```

## Logische Werte

Konstanten: `true` und `false`.

## Zeichenketten

Beispiele
```
""
"\\ (write to backslashes to get one) is the escape character for \\ \$ \" \t \r \n"
"String with \"nested\" quotes"
"\$ has a special meaning: $VARIABLE, ${MY-VARIABLE} and $($VAR + 1)"
"\t means the TAB control character"
"\r means the CR control character"
"\n means the LF control character"
```

String werden in Anführungszeichen gesetzt.
Um die besondere Bedeutung der drei Zeichen " \ $ zu unterdrücken,
stellt man denen ein \ voran.
Die Steuerzeichen \t \r \n werden so geschrieben.
Andere Zeichen sind nach \ nicht erlaubt.

Der Code in zum Schreiben eines Strings der Ausdruckssprache
([Expression.scala](https://github.com/sos-berlin/js7/blob/3cf58799748b4c7f87736a6eab77ca95d5a4ffbd/js7-data/shared/src/main/scala/js7/data/value/expression/Expression.scala#L158-L169)):
```scala
val sb = new StringBuilder(64)
sb.append('"')
string foreach {
  case '\\' => sb.append("\\\\")
  case '\"' => sb.append("\\\"")
  case '\t' => sb.append("\\t")
  case '\r' => sb.append("\\r")
  case '\n' => sb.append("\\n")
  case '$' => sb.append("\\$")
  case c => sb.append(c)
}
sb.append('"')
```
`JExpression.quoteString(String)` wandelt den String in einem parsbaren String der Ausdruckssprache.

Daneben gibt es die manchmal besser lesbare Schreibweise mit Apostrophen ',
für nicht-leere Strings, die kein Apostroph und kein anderes Steuerzeichen
als LF (und CR) enthalten. \ ist hier kein Fluchtzeichen. CR wird ignoriert.
Die Schreibweise kann nicht alle Strings darstellen.


## Zahlen
Konstanten: Zahlen werden als Java `BigDecimal` dargestellt.
Nur ganze Zahlen sind (bislang) möglich.

## Operationen

* `$NAME`, `${NAME}` — Abruf einer Variablen

  Ähnlich wie zum Beispiel in Bash kann eine Variablen mit $ oder mit ${} abgerufen werden.
  Im ersten Fall sind alle Namen möglich, die den Java-Regeln entsprechen.
  Mit geschwungenen Klammer sind alle Zeichen außer } möglich.

  ⚠️ Einige Sonderzeichen werden vielleicht zukünftig wie in Bash interpretiert;
  schön wäre etwa die knappe Schreibweise ${NAME:-DEFAULT}

  Es ist ein Fehler, wenn die Variable nicht bekannt ist.

  Mehr Information bei der Funktion `variable()`.

* `<`, `<=`, `==`, `!=`, `>=`, `>`

  Ergebnistyp ist `Boolean`.
  Verglichen werden können Zahlen und Strings.
  Beide Seiten müssen vom selben Typ sein.
  Eine Zahl kann also nicht mit einem String verglichen werden.
  Unterschiedliche Typen lassen den Workflow abbrechen (mit `try/catch` abfangbar).

* `+`, `-`
Addition und Subtraktion zweier Zahlen.

* `++`
Konkatenation zweier Strings.
Operanden vom Typ Number und Boolean werden zu einem String gewandelt.

* `variable(NAME, label=LABEL, job=JOB, default=DEFAULT)`
   * `NAME` ist ein String-Ausdruck für den Namen der Variabeln.
   * `label=LABEL` (optional) ist das Label einer Anweisung, in deren Ergebnis der Name erwartet wird.
     Das Label wird nicht in Anführungszeichen gesetzt, Beispiel: `label=A`.
   * `label=JOB` (optional) der Name eines Jobs, in dessen Ergebnis der Name erwartet wird.
     Der Name wird nicht in Anführungszeichen gesetzt, Beispiel: `job=MYJOB`.
   * ❓ _Brauchen wir das alles?_
* `EXPR.toNumber` — in eine Zahl konvertieren
   Wenn der Ausdruck `EXPR` bereits eine Zahl ist, dann passiert nichts.
   Ein String dagegen wird in eine Zahl gewandelt.
   Bei einem Fehler bricht die Anweisung ab.

  ❓ _ist das eine gute Syntax für die Anwender?
  Ich habe das von Scala übernommen, in guter objekt-orientierter Tradition,
  und es macht einen langen Ausdruck lesbarer, weil man von links nach rechts lesen kann.
  Sonst ist die funktionale Notation üblich: `toNumber(EXPR)`.
* `EXPR.toString`
  Wandelt den Wert einen String.
  Kann nicht scheitern.
