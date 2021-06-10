# v2.0.0-alpha.20210212

* out und err für interne Jobs.
Das sind eigene Kanäle, _nicht_ stdout und stderr des Agenten.
Dazu erhält `BlockingInternalJob` ein neues `JOrderProcess` mit den Methoden
  * `out()` und `err()` als `PrinterWriter`
  * `outWriter()` und `errWriter()` für die zugrunde liegenden `Writer`.

Es ist deutlich effizienter, viel Text auf einmal statt in vielen Häppchen zu schreiben.
Jedes `printLn()` bzw. `write()` kann ein Event erzeugen.

# v2.0.0-alpha.20210211

* Shell-Jobs mit v1Compatible erhalten jetzt auch die
im Workflow deklarierten Default-Parameter,
obwohl JobScheduler 1 das kannte.
* Außerdem kleine Verbesserungen und eine Optimierung durch weniger Threads.

# v2.0.0-alpha.20210208

* Proxy
  * Der Aufruf `x509WithSignerId` ersetzt `x509WithSignedId`.
  * Wegen der neuen internen Java-Jobs haben sich mehrere Klassennamen geändert.
  * Value.of akzeptiert jetzt java.math.Bigdecimal
  * NumberValue.toBigDecimal und .toJava liefern java.math.Bigdecimal
* Interne Java-Jobs
  * Betrifft die Verwendung des Proxys


## Interne Java-Jobs

### Neues Subprojekt js7-data-for-java
* Das neue Subprojekt js7-data-for-java nimmt die Java-Adapter aus dem Proxy auf,
  sodass sie auch für interne Java-Jobs verwendet werden können.
  Die betroffenen Klassen (im Wesentlichen die J-Klassen) sind in neuen Packages.
  Eure Import-Anweisungen müssten wie folgt angepasst werden:

  Alt | Neu
  --- | ---
  js7.proxy.javaapi.data.command.             |  js7.data_for_java.command.
  js7.proxy.javaapi.data.controller.          |  js7.data_for_java.controller.
  js7.proxy.javaapi.data.event.JKeyedEvent.   |  js7.data_for_java.event.JKeyedEvent.
  js7.proxy.javaapi.data.auth.                |  js7.data_for_java.auth.
  js7.proxy.javaapi.data.order.               |  js7.data_for_java.order.
  js7.proxy.javaapi.data.problem.             |  js7.data_for_java.problem.
  js7.proxy.javaapi.data.workflow.            |  js7.data_for_java.workflow.
  js7.proxy.javaapi.data.common.VavrUtils.    |  js7.data_for_java.vavr.VavrUtils.

### Neues Subproject js7-executor-for-java

#### InternalExecutable

Näheres zur den internen Java-Jobs unter [Executable](items/workflow/Executable.md),
Abschnitt InternalExecutable.

````json
{
  "TYPE": "InternalExecutable",
  "className": "com.example.MyInternalJavaJob",
  "jobArguments": {
    "stringArgument": "An Argument for the intantiated class",
    "numericArgument": 3
  },
  "arguments": {
    "arg": "$$ARG",
    "numericArg": "7"
  }
}
````

```java
package js7.executor.forjava.internal.tests;

import ...

public final class EmptyBlockingInternalJob implements BlockingInternalJob
{
    public JOrderResult processOrder(JOrderContext context) {
        return JOrderResult.of(emptyMap());
    }
}
```

----------------------------------------------------------------------------------------------------
# v2.0.0-alpha.20210131

* Fix: Die Lock-Anweisung erlaubt jetzt Jobs verschiedener Agenten
* Fix: Ein leerer Workflow blockiert nicht mehr den Auftrag. Der Auftrag wird nun nach dem Start sofort beendet.
* Fix: defaultArguments von Job und Execute-Anweisung werden jetzt korrekt berücksichtigt (Execute hat Vorrang vor dem Job).
* Experimentell: InternalExecutable zum Ausführen interner, als Klasse definierter Jobs; zunächst für Scala

## InternalExecutable
`InternalExecutable` erlaubt es (zunächst experimentell) einen Job als JVM-Klasse mit Scala-Schnittstelle, später auch mit Java-Schnittstelle zu definieren. So ein Job startet also keinen Prozess, sondern läuft in der JVM des Agenten.

Beispiel für einen Aufruf:
```json
"executable": {
  "TYPE": "InternalExecutable",
  "className": "js7.tests.jobs.EmptyJob"
}
```
Der in JS7 bereits vordefinierte interne Job `js7.tests.jobs.EmptyJob` tut nichts.

----------------------------------------------------------------------------------------------------
# v2.0.0-alpha.20210127

## Deklarierte Auftragsparameter

Ein Workflow kann von seinen Aufträgen bestimmte Parameter (im Feld `arguments` des Auftrags) verlangen.
Für hinzufügende Aufträge gilt:
* Ein Auftragsparameter ist optional, wenn ein Default-Wert bestimmt ist.
* Der Auftrag muss für alle Parameter ohne Default einen Wert angeben.
* Der Typ des Werts muss mit dem deklarierten Typ übereinstimmen.
* Ein Auftrag mit anderen Parametern wird ablehnt.

Beispiel in JSON den beiden Parametern
* `myString: String = "DEFAULT"`
* `myNumber: Number` (nicht optional)

```json
{
  "path": "PARAMETERIZED-WORKFLOW",
  "orderRequirements": {
    "parameters": {
      "myString": {
        "type": "String",
        "default": "DEFAULT"
      },
      "myNumber": {
        "type": "Number"
      }
    }
  },
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentPath": "AGENT",
        "executable": {
          "TYPE": "ScriptExecutable",
          "script": "#!/usr/bin/env bash\nset -euo pipefail\necho \"STRING=$STRING\"\necho \"NUMBER=$NUMBER\"\n",
          "env": {
            "STRING": "$myString",
            "NUMBER": "$myNumber"
          }
        },
        "parallelism": 1
      }
    }
  ]
}
```
