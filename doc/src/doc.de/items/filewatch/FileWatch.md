# FileWatch

Ein FileWatch überwacht ein Verzeichnis und erzeugt für jede Datei darin einen Auftrag.

Alle Verzeichniseinträge (außer Verzeichniseinträge "`.`" und "`..`") werden beachtet.

- Der FileWatch erzeugt mit jeder erkannten Datei einen Auftrag.
- Der Auftrag hat den Parameter `file` mit dem absoluten Pfad der Datei.
- Zur jeder Datei wird genau ein Auftrag erzeugt.
- Am Ende des Workflows löscht JS7 den Auftrag erst, wenn die Datei gelöscht worden ist.
- Die Datei kann während des Laufs durch den Workflow gelöscht und wieder angelegt werden.
  Falls JS7 das erkennt, dann erzeugt JS7 nach Beendigung des akuellen Auftrags
  einen neuen Auftrag für die erneut angelegte Datei.
  JS7 kann das nicht zuverlässig erkennen,
  falls der Agent gerade nicht hinsieht, während die Datei verschwindet und wieder erscheint,
  zum Beispiel
  - bei einem Neustart des Agenten,
  - bei zu vielen Änderungen im Verzeichnis (Überlauf),
  - je nach Betriebssystem und
  - periodisch, wenn der die Verzeichnisüberwachung erneuert wird (zur Zeit jede Minute).

Um eventuelle Probleme mit dem Verzeichnis zu erkennen,
erneuert der Agent die Verzeichnisüberwachung periodisch (zur Zeit jede Minute).
Dadurch werden
- Umbenennungen des Verzeichnisses (bei Unix-Dateisystemen) erkannt
- Fehler in der Verzeichnisüberwachung umgangen (zum Beispiel bei Verzeichnissen im Netzwerk).

Verzeichsnisse im Netzwerk können überwacht werden.
JS7 verträgt Verzögerungen (besonders bei Störungen) bei der Verzeichnisüberwachung.
Die Überwachung läuft in gesonderten Threads.

Wenn man den FileWatch erneuert und das  anderes Verzeichnis oder einen anderen Agenten angibt,
dann gelten alle bisher erkannten Dateien als verschwunden,
so dass die laufenden Aufträge nach Beendigung gelöscht werden.

Parameter:
### `agentPath`
Das Verzeichnis wird von einem Agenten überwacht.

### `directory`
Der (möglichst absolute) Pfad des Verzeichnisses.

### `pattern`
(Optional) nur Dateinamen, die dem regulärem Ausdruck entsprechen, werden beachtet.

Beispiele:
- `.*` jeder Dateiname wird akzeptiert
- `prefix-(.*)\.csv` Nur Dateinamen mit dem Präfix `prefix-` und der Dateinamensendung `.csv` werden beachtet.
Der mittlere Teil kann für die OrderId verwendet werden (als `$1` in `orderIdExpression`).

Allgemeines zu regulären Audrücken siehe https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html.

### `orderIdExpression`
(Optional) Ausdruck, die die OrderId errechnet

Die Ausdruckssprache kennt hier einige Besonderheiten:

#### Funktion now(format=FORMAT, timezone=TIMEZONE)
(Wie auch für JobResource.)
Liefert den aktuellen Zeitpunkt (also bei Entstehung des Events ExternalOrderArised),
formatiert mit DateTimeFormatter und übersetzt in die angegebene Zeitzone (UTC, wenn nicht angegeben).

Der Ausdruck im JSON-Beispiel
`now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
liefert zum Beispiel `"2021-04-23"`.

Beispiele
- `now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
- `now(format='yyyy-MM-dd')`  (UTC-Zeitzone)

#### Variablen
- `$js7EpochSecond`: Zahl der Sekunden seit dem 1. Januar 1970 UTC.
- `$js7EpochMilli`: Zahl der Millisekunden seit dem 1. Januar 1970 UTC.
- `$0`: Gruppe 0 des regulären Ausdrucks, also der gesamte Dateiname
- `$1`, `$2`, ... Die _capturing group_ des Ausdrucks (`(.*)` im obigen Beispiel).

### `delay`
(optional) Dauer in Sekunden, die Datei unverändert bleibt, bevor ein Auftrag erzeugt wird.
Kann verwendet werden, wenn die Datei nicht atomar übertragen wird,
und man annehmen muss, dass nach der Dauer die Datei wohl vollständig bereitgestellt ist
— oder die Übertragung abbrach, was nicht zu unterscheiden ist.


Beispiele (in JSON):
```json
{
  "TYPE": "FileWatch",
  "path": "MY-FILEWATCH",
  "workflowPath": "WORKFLOW",
  "agentPath": "AGENT",
  "directory": "/.../DIRECTORY"
}
```

```json
{
  "TYPE": "FileWatch",
  "path": "MY-FILEWATCH",
  "workflowPath": "WORKFLOW",
  "agentPath": "AGENT",
  "directory": "/.../DIRECTORY",
  "pattern": "file-(.+)\\.csv",
  "orderIdExpression": "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$js7EpochSecond-$orderWatchId:$1\"",
  "delay": 2
}
```

## Vordefinierter Job `DeleteJob`

Zum Testen. Erfordert die Jar `js7-tests.jar` (die eher nicht ausgeliefert wird).

````json
{
  "TYPE": "InternalExecutable",
  "className": "js7.tests.jobs.DeleteFileJob",
  "jobArguments": {},
  "arguments": {}
}
````
