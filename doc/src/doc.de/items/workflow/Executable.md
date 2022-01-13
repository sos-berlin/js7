# Executable

Beispiel in JSON
````json
{
  "TYPE": "PathExecutable",
  "path": "my-script.sh",
  "env": {
    "MY_ENV": "$myOrderArgument"
  }
}
````
Ein Executable wird im WorkflowJob und in der Execute-Anweisung verwendet.

Executable hat folgende Subtypen:

- ShellScriptExecutable
- PathExecutable
- CommandLineExecutable

Jeder dieser Subtypen kennt folgende Parameter.

* `v1Compatible`

  Default: `false`

  `true`, um den Job wie mit JobScheduler 1 zu starten.
  * Alle Auftragsparameter werden als Umgebungsvariablen übergeben.
    Die Namen der Umgebungsvariablen sind komplett in Großbuchstaben
    und beginnen mit `SCHEDULER_PARAM_`.
  * Die Datei für die Rückgabewerte ist in der
    Umgebungsvariablen `SCHEDULER_RETURN_VALUES` angegeben.
  * Der Paramenter `env` wird nicht interpretiert.

**Felder**

* `env` (optional)

  Map\[String, ExpressionString]

  Auf der linken Seite stehen die Namen der zu setzenden Umgebungsvariablen.
  Auf der rechten Seite sind Strings mit [Ausdrücken](../../expression/Expression.md).
  Vor dem Start des Jobs errechnet JS7 den Wert jedes Ausdrucks und
  weist ihn der genannten Umgebungsvariablen zu (in einen String konvertiert).

  Beispiel:
  ```json
  {
    "CONSTANT": "\"STRING-CONSTANT\"",
    "VAR": "$VAR",
    "NUMBER": "7",
    "RC": "$returnCode + 1"
  }
  ```

  Zur Ausdruckssprache siehe [Ausdrücke](../../expression/Expression.md).

* `returnCodeMeaning`

  Die Returncodes (Exitcode eines System-Prozesses),
  die als Erfolg oder Versagen gewertet werden sollen.
  Default ist (wie sonst üblich), dass 0 Erfolg bedeutet und alles andere Versagen:
  `success=[0]`

  * `success=[...]`

    Liste ganzzahliger Returncodes, die als Erfolg gewertet werden.
    Alle anderen werden als Versagen gewertet.

  * `failure=[...]`
    Liste ganzzahliger Returncodes, die als Versagen gewertet werden.
    Alle anderen werden als Erfolg gewertet.

* `v1Compatible` (default `false`)

  Ob Parameter und Rückgabewerte wie JobScheduler übergeben werden sollen.



## ShellScriptExecutable

Erfordert in der Konfigurationsdatei `agent.conf` des Agenten die Einstellung

```
js7.job.execution.signed-script-injection-allowed = on
```
Die Einstellung erlaubt es, ausführbaren Code in den Agenten zu schleusen.
Der Workflow, der den Code enthält, muss signiert sein.

```json
{
  "TYPE": "ShellScriptExecutable",
  "script": "#!/usr/bin/env bash\nset -euo pipefail\n\necho \"HELLO!\"",
  "env": {
    "ENV-VAR": "$$VAR",
    "NUMBER": "7"
  },
  "login": {
    "credentialKey": "CREDENTIAL-KEY",
    "withUserProfile": true
  },
  "v1Compatible": true
}
```

*Parameter*
* `script` — Das Skript
* `env` — optional, die errechneten Umgebungsvariablen für den Prozess
* `login` — optional für Windows-Logon mit den Parametern
  * `credentialKey` — der in Windows hinterlegte Credential Key
  * `withUserProfile` — true/false, ob für den Prozess das Nutzer-Profil geladen werden soll.
* `v1Compatible` — optional, true/false, zur Kompatibilität mit Jobs von JobScheduler 1.


## PathExecutable

Eine ausführbare Datei.
Wenn der Pfad der Datei nicht mit einem Schrägstrich oder einem Backslash beginnt,
dann bezeichnet er eine Datei im Verzeichnis `CONFIG/executable`.

⚠️ Windows-Dateinamen mit Laufwerkbuchstaben werden nicht korrekt erkannt.

```json
{
  "TYPE": "PathExecutable",
  "path": "subdirectory/my-executable.sh"
}
```

## CommandLineExecutable

...

## InternalExecutable

Interne Jobs sind Java-Klassen, die der Agent in seine JVM lädt und dort lokal ausführt.
Sie sind vorsichtig zu benutzen, denn sie laufen im Prozess des Agenten und
können den Betrieb des Agenten stören.

Interne Jobs können eines der beiden folgenden Interfaces implementieren.
* `JInternalJob` zur Ausführung als `Future` mit einem Threadpool (etwa Javas `commonPool`),
* `BlockingInternalJob` zur einfachen Implementierung ohne Future.


### JInternalJob

Minimales Beispiel für einen internen Job, der nichts tut:

```java
package js7.launcher.forjava.internal.tests;

import java.util.concurrent.CompletableFuture;
import js7.launcher.forjava.internal.JInternalJob;
import js7.launcher.forjava.internal.JOrderProcess;
import js7.data_for_java.order.JOutcome;

/** Skeleton for a JInternalJob implementation. */
public final class EmptyJInternalJob implements JInternalJob {
  public JOrderProcess toProcessOrder(Step step) {
    return JOrderProcess.of(
      CompletableFuture.supplyAsync(
        () -> JOutcome.succeeded()));
  }
}
```
`processOrder` liefert sofort ein `OrderProcess` zurück,
dessen einziges Element ein `CompletionStage<JOutcome.Completed>` ist,
das aus `Step` ein `JOutcome.Completed` berechnet.
`processOrder` kann noch in einem Thread des Agenten laufen und soll deshalb nichts anderes tun, als eine `Future` (genauer: `CompletableStage`) zu starten und
ohne deren Ergebnis abzuwarten in `JOrderProces` zurückzugeben.

Die üblichen Regeln für Futures und den betreibenden `Executor` gelten.
(Dokumentation dazu vielleicht hier: https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CompletableFuture.html)

Der Parameter `Step` ist unten beschrieben.

`JOutcome.succeeded` erwartet optional eine `Map<String, Value>` mit dem Ergebnis der Ausführung.

#### JJobContext

Eine JInternalJob-Klasse kann einen Konstruktor mit dem Parameter `JJobContext` definieren,
der im Wesentlichen das folgende Feld enthält:
* `jobArguments`, eine `Map<String, Value>` mit den für den Job (nicht Auftragsschritt)
  bestimmten Parametern.

#### Step
`Step` stellt für einen Auftragsschritt bereit:
* `arguments`, eine `Map<String, Value>` mit den deklarieren Parametern (s.u.),
* `order`, ein `JOrder`, also der Auftrag, und
* `workflow`, ein `JWorkflow`, also der Workflow, in dem sich der Auftrag befindet.

In den meisten Fällen wird man mit `arguments`, den deklarierenten Parametern, auskommen.

### BlockingInternalJob

Ein Job, der `BlockingInternalJob` implementiert,
verzichtet auf asynchrone Ausführung mit `Future`
und führt den Schritt stattdessen synchron durch.
Der Agent lässt die Aufrufe dieser Klasse in einem
unbegrenzten Threadpool ausführen.

⚠️ Viele parallel ausgeführte Auftragsschritte könnten
den Betrieb des Agenten beeinträchtigen,
wenn dessen Threads nicht mehr genug CPU zugeteilt bekommen.
Die `parallelism` der Jobs sollte mit Bedacht eingestellt sein.

Minimales Beispiel für einen internen Job, der nichts tut:

```java
package js7.launcher.forjava.internal.tests;

import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.BlockingInternalJob;

public final class EmptyBlockingInternalJob implements BlockingInternalJob
{
    public OrderProcess toProcessOrder(Step step) {
        return () -> JOutcome.succeeded();
    }
}
```

Beispiel für einen mit `CancelOrder` abbrechenbaren Job:

```java
package js7.tests.internaljob;

import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.BlockingInternalJob;

public final class JCancelableJob implements BlockingInternalJob
{
    public OrderProcess toProcessOrder(Step step)
    {
        // Do nothing but return an OrderProcess

        return new OrderProcess() {
            volatile boolean canceled = false;

            public JOutcome.Completed run() throws InterruptedException {
                // Process the order here
                while (!canceled) {
                    Thread.sleep(10);
                }
                return JOutcome.failed("Canceled");
            }

            public void cancel(boolean immediately) {
                canceled = true;
            }
        };
    }
}
```

#### JJobContext

Eine BlockingInternalJob-Klasse kann einen Konstruktor mit dem Parameter `JJobContext` definieren,
der im Wesentlichen das folgende Feld enthält:
* `jobArguments`, eine `Map<String, Value>` mit den für den Job (nicht Auftragsschritt)
  bestimmten Parametern.

#### Step
`Step` (eigentlich `JBlockingJob.Step`) stellt für einen Auftragsschritt bereit:
* `arguments`: eine `Map<String, Value>` mit den deklarieren Parametern (s.u.),
* `order`: ein `JOrder`, also der Auftrag, und
* `workflow`: ein `JWorkflow`, also der Workflow, in dem sich der Auftrag befindet.
* `outWriter` und `errWriter` die Out- und Err-Känäle
* `out`, `err`: die Out- und Err-Kanäle als `PrinterWriter`.

In den meisten Fällen wird man mit `arguments` auskommen.

Die Out- und Err-Kanäle sind ungepuffert und
haben bei jedem `write` bzw. `println` einen Kontextwechsel
und oft ein Event zur Folge.
Eine Ausgabe mehrerer Zeilen wird effizienter in einem Aufruf übergeben,
oder man verwendet einen ausreichend gepufferten `BufferedWriter`.

### Fehlerbehandlung

Bei einer Exception scheitert der Auftragsschritt.

`JOutcome.Completed` hat zwei Subtypen:

* `JOutcome.Succeeded` für den Erfolgsfall.
  Die Methoden dafür sind
  * `JOutcome.succeeded()` und
  * `JOutcome.succeeded(Map<String, Value>)`
* `JOutcome.Failed` für den Fehlerfall.
  Der Typ lässt den Auftragsschritt scheitern.
  Die Methoden dafür sind
  * `JOutcome.failed(String message)` und
  * `JOutcome.failed(String message, Map<String, Value>)`


#### JSON

In einem WorkflowJob wird das `executable` zum Beispiel so angegeben:

````json
{
  "TYPE": "InternalExecutable",
  "className": "com.example.MyInternalJavaJob",
  "jobArguments": {
    "stringArgument": "An Argument for the intantiated class",
    "numericArgument": 3
  },
  "arguments": {
    "arg": "$ARG",
    "numericArg": "7"
  }
}
````

* `jobArguments` sind die konstanten Werte,
  die der Job im optionalen Konstruktor per `JJobContext` erhält.
* `arguments` sind die Parameter für jeden Auftragsschritt.
Auf der rechten Seite werden Ausdrücke erwartet,
die den jeweiligen Parameter aus dem Auftrag errechnen
(hier "arg" aus dem letzten Wert für "ARG" im Auftrag).
