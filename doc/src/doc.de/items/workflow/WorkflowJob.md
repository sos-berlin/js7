# WorkflowJob

Beispiel in JSON:
```json
{
  "agentPath": "AGENT",
  "executable": {
    "TYPE": "ScriptExecutable",
    "script": "echo \"hello world1\"\n"
  },
  "defaultArguments": {
    "NAME": "VALUE",
    "NUMBER": 7
  },
  "returnCodeMeaning": {
    "success": [ 0 ]
  },
  "taskLimit": 1,
  "sigkillDelay": 15
}
```
**Felder**
* `agentPath`
* `executable`

  Ein [Executable](Executable.md)

* `defaultArguments`

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

* `taskLimit`

  Wie oft dieser Job gleichzeitig ausgeführt werden kann.

  Default: `taskLimit=1`

* `sigKillDelay`

  Nur Unix.
  Wenn nach dem Versuch, einen Prozess abzubrechen,
  der Prozess nach der angegebenen Zeitspanne noch läuft,
  dann sendet JS7 dem Prozess ein `SIGKILL`-Signal.

  Default: Die [Einstellung](../../settings.md) `js7.job.execution.sigkill-delay`
  in der Datei `agent.conf`.
