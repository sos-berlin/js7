# JobResource

Einem Job lassen sich JobResourcen zugeordnen.
Eine JobResoure enthält

* `variables`, errechnete benannte Variablen und
* `env`, errechnete Umgebungsvariablen für Shell-Jobs.

`variables` und `env` sind Name-Ausdruck-Paare.

In `env` können die Namen aus `variables` verwendet werden.
Damit kann eine Einstellung in `variables` vorgenommen
und in JVM- wie Shell-Jobs verwendet werden.

Beispiel in JSON:

```json
{
  "path": "MY-JOB-RESOURCE",
  "TYPE": "JobResource",
  "variables": {
   "stringSetting": "\"STRING\"",
   "numberSetting": "100"
  },
  "env": {
    "MYENV": "\"VALUE\"",
    "MYPATH": "\"/bin:\" ++ env(\"PATH\")"
  }
}
```

Funktionen und Variablen der Ausdruckssprache speziell für JobResource:

### Funktion env(name, default)
liefert den Wert einer Umgebungsvariablen des Agenten-Prozesses.
Die Funktion scheitert, wenn die Umgebungsvariable unbekannt ist und kein default-Wert angegenben ist.

### Funktion now(format=FORMAT, timezone=TIMEZONE)
(Wie schon für FileWatch.)
Liefert den aktuellen Zeitpunkt (also bei Entstehung des Events ExternalOrderAppeared),
formatiert mit DateTimeFormatter und übersetzt in die angegebene Zeitzone (UTC, wenn nicht angegeben).

Der Ausdruck im JSON-Beispiel
`now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
liefert zum Beispiel `"2021-04-23"`.

Beispiele
- `now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
- `now(format='yyyy-MM-dd')`  (UTC-Zeitzone)

### Funktion scheduledOrEmpty(format=FORMAT, timezone=TIMEZONE)
Liefert analog zur Funktion `now` die geplante Startzeit des Auftrags,
oder einen leeren String, wenn der Auftrag keine geplante Startzeit hat.

Beispiele
- `scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ss')`  (UTC-Zeitzone)

### Variablen
- `$js7EpochSecond`: Zahl der Sekunden seit dem 1. Januar 1970 UTC.
- `$js7EpochMilli`: Zahl der Millisekunden seit dem 1. Januar 1970 UTC.
- `$js7OrderId`
- `$js7JobName`
- `$js7Label`: Das Label oder der leere String
- `$js7WorkflowPosition`: WorkflowId (Path + VersionId) und Position im Workflow als String
- `$js7WorkflowPath`
- `$js7ControllerId`

JobResourcen können so in einem Job referenziert werden:
```json
{
  "path": "WORKFLOW-ENV",
  "versionId": "1.0",
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentPath": "AGENT",
        "jobResourcePaths": [
          "MY-JOB-RESOURCE"
        ],
        "executable": {
          "TYPE": "ShellScriptExecutable",
          "script": "#!/usr/bin/env bash\nset -euo pipefail\necho MYENV=/$MYENV/\n"
        }
      }
    }
  ]
}
```
Wenn mehrere JobResourcen angegeben sind, die gleiche Namen für Umgebungsvariablen enthalten,
dann gelten die _zuerst_ angegeben (links hat Vorrang).
Dabei ist auch unter Windows ist die Groß/Kleinschreibung relevant.
Für Jobs, die unter Windows laufen,
sollte für Umgebungsvariablen eine einheitliche Großschreibung gewählt werden.

### Beispiel für die allgemeinen Umgebungsvariablen der SOS
````json
{
  "path": "SosStandard",
  "env": {
    "JS7_ORDER_ID": "$js7OrderId",
    "JS7_WORKFLOW_POSITION": "$js7WorkflowPosition",
    "JS7_WORKFLOW_NAME": "$js7WorkflowPath",
    "JS7_JOB_NAME": "$js7JobName",
    "JS7_CONTROLLER_ID": "$js7ControllerId",
    "JS7_TASKSTART_DATE": "now(format='yyyy-MM-dd HH:mm:ssZ')",
    "JS7_TASKSTART_YEAR": "now(format='yyyy')",
    "JS7_TASKSTART_MONTH": "now(format='MM')",
    "JS7_TASKSTART_DAY": "now(format='dd')",
    "JS7_TASKSTART_HOUR": "now(format='HH')",
    "JS7_TASKSTART_MINUTE": "now(format='mm')",
    "JS7_TASKSTART_SECOND": "now(format='ss')",
    "JS7_SCHEDULED_DATE": "scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ')",
    "JS7_SCHEDULED_YEAR": "scheduledOrEmpty(format='yyyy')",
    "JS7_SCHEDULED_MONTH": "scheduledOrEmpty(format='MM')",
    "JS7_SCHEDULED_HOUR": "scheduledOrEmpty(format='HH')",
    "JS7_SCHEDULED_DAY": "scheduledOrEmpty(format='dd')",
    "JS7_SCHEDULED_SECOND": "scheduledOrEmpty(format='ss')",
    "JS7_SCHEDULED_MINUTE": "scheduledOrEmpty(format='mm')"
  }
}
````


### Abruf am Workflow

JobResourcen, die jedem Job eines Workflows mitgegeben werden sollen,
können am Workflow mit dem Parameter `jobResourcePaths` bestimmt werden.
Sie werden der Liste der JobResourcen am Job angehängt.


### Nutzung in JVM-Jobs

```java
public final class MyInternalJob implements BlockingInternalJob
{
    public OrderProcess toOrderProcess(Step step) {
        return () -> {
          Map<JobResourcePath,Map<String,Value>> jobResourceToNameToValue =
            step.jobResourceToNameToValue();
          Either<Problem,Value> checkedValue =
            step.byJobResourceAndName(JobResourcePath.of("A"), "stringSetting");
        };
    }
}
```
