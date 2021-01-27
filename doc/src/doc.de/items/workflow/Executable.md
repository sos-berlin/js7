# Executable

Beispiel
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

- ScriptExecutable
- ExecuteblePath
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
  Auf der rechten Seite sind Strings mit [Ausdrücken](expression/expression.md).
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

  Zur Ausdruckssprache siehe [Ausdrücke](expression/expression.md).


* `v1Compatible` (default `false`)

  Ob Parameter und Rückgabewerte wie JobScheduler übergeben werden sollen.



## ScriptExecutable

Erfordert in der Konfigurationsdatei `agent.conf` des Agenten die Einstellung

```
js7.job.execution.signed-script-injection-allowed = on
```
Die Einstellung erlaubt es, ausführbaren Code in den Agenten zu schleusen.
Der Workflow, der den Code enthält, muss signiert sein.

```json
{
  "TYPE": "ScriptExecutable",
  "script": "#!/usr/bin/env bash\nset -euo pipefail\n\necho \"HELLO!\""
}
```

*Parameter*
*script


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
