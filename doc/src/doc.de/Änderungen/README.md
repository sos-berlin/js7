# Änderungen

## 2018-03-19

### Zugriff auf vorherige Ergnisse vorheriger Auftragsschritte

#### Umbennungen

Das Feld ```argument``` enthält die ursprünglichen Parameter des Auftrags, 
wie sie mit dem Auftrag eingespeist worden sind.

- Webservice ```POST master/api/order``` (```FreshOrder```):
- ```OrderAddedFat``` und  ```OrderForkedFat```
- ```OrderProcessingStartedFat```, ```OrderProcessedFat```, ```OrderJoinedFat```
- GraphQL

In GraphQL heißt das Feld ```outcome``` jetzt ```lastOutcome```.


```json
{
  "id": "ORDER-ID",
  "workflowPath": "/WORKFLOW",
  "scheduledFor": 1488888000000,
  "arguments": {
    "KEY": "VALUE"
  }
}
```


## 2018-03-18

### Syntax für fork-Anweisung geändert

- Geschweifte statt runde Klammern
- Doppelpunkt nach der Branch-ID

```
fork {
  "🥕": {
    job carrot;
  },
  "🍋": {
    job lemon;
  }
}
'''

## 2019-03-14 (2)

### JSON für Execute und Job-Definition geändert

Das Feld ```executablePath: "/EXECUTABLE"``` habe ich ersetzt durch

```json
"executable": {
  "TYPE": "ExecutablePath",
  "path": "/EXECUTABLE"
}
```

Ein Workflow mit direktem Aufruf eines Skript und mit Definition eines Jobs sieht also so aus:

```json
{
  "instructions": [ 
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentRefPath": "/AGENT",
        "executable": {
          "TYPE": "ExecutablePath",
          "path": "/EXECUTABLE"
        },
        "taskLimit": 1
      }
    },
    ...  
  ],
  "jobs": {
    "MYJOB": {
      "agentRefPath": "/AGENT",
      "executable": {
        "TYPE": "ExecutablePath",
        "path": "/EXECUTABLE"
      },
      "taskLimit": 1
    }
  }
}
```

### Skript im Job

TODO

## 2019-03-14

### Geändertes JSON für Aufruf eines Jobs

Das Feld ```name``` heißt jetzt ```jobName```:

```json
{
  "TYPE": "Execute.Named",
  "jobName": "JOB"
}
```

### Klasse `Agent` heißt jetzt `AgentRef` 

Dementsprechend heißt das Feld ```agentPath``` jetzt ```agentRefPath```.
Damit sind Agent und Verweis auf einen Agenten klar unterschieden. 

Betroffen sind 
  - Anweisung ```Execute.Anonymous````
  - Anweisung ```Job```
  - Event ```OrderProcessingStartedFat```
  - GraphQL 
  - Webservice ```master/api/agent/?return=AgentRef``
  - Feld ```TYPE``` im Agentenverweis selbst (Beispiel folgt)
  - Die JSON-Dateinamen enden auf ```.agentref.json``
  
### GraphQL kennt das Feld ```agentId``` im Auftrag nicht mehr

Es wird stets der aktuelle Agentenverweis genutzt, deshalb fehlt die Versionsnummer.
  
### Neue Variable ```${jobscheduler.config-directory}``` für .conf-Dateien

Beispiel:
```
jobscheduler.configuration.trusted-signature-keys {
  PGP = ${jobscheduler.config-directory}"/private/trusted-pgp-keys.asc"
}
```
  
### Der Master kennt das Verzeichnis ```config/live``` nicht mehr 
 
Workflows und Agentenverweis können nur mit den Kommandos ```ReplaceRepo``` und ```UpdateRepo``` übergeben werden.
Mit beiden Kommandos können Agentenverweise und Workflows hinzugefügt und geändert werden.
Workflows können auch gelöscht werden.

❗️ Agentverweise lassen vorerst nicht löschen. 
Das Kommando wird abgelehnt, wenn dabei ein Agentenverweis gelöscht wird. 

Wie bisher kann der Master neu gestartet werden, ohne dass er die Workflows und Agentenverweise vergisst.

#### Version

Jede Übergabe einer vollständigen (ReplaceRepo) oder geänderten (UpdateRepo) Konfiguration (aus Agentenverweisen und Workflows)
bildet eine neue Version.
Der Master hält alle Versionen in einem internen Repository (kurz Repo).
Jede Version wird mit einer VersionId identifiziert.

Die VersionId sollte aufsteigend vergeben werden. 
Sobald ich eine Regel habe, werden ich sie in den Master einbauen.

#### Versionierte Workflows

Vorhandene Aufträge laufen in ihrer Version des Workflows weiter. 
Eine Änderung oder Löschung eines Workflows wirkt sich nicht auf vorhandene Aufträge aus.
Das gilt auch für wartende Aufträge 
(das könnte ich ändern: die sollten im neuesten Workflow starten, wenn der nicht gelöscht worden ist).

#### Versionierte Agentenverweise

Die Agentenverweise werden zwar auch versioniert, aber es gilt stets die aktuelle Version.
Wenn ein Agentenverweise geändert wird (d.h. der URI des Agenten wird geändert), dann wirkt das sofort, für alle Aufträge.

Der URI eines Agenten kann aus zwei Gründen geändert werden:
- Der bloße Name hat sich geändert, ohne dass der Agent selbst angerührt wird. Zum Beispiel 
   - Anderer Hostname 
   - Agent wird über Proxy angesprochen
   - HTTPS statt HTTP
- Der Agent zieht von einer Maschine auf eine andere um
  - Dabei muss sein Zustand, also das Verzeichnis ```state``` mit umziehen.
  - Der umgezogene Agent kann nicht mit leeren Zustand gestartet werden.
    Das wird den Master verwirren und macht die Aufträge unbrauchbar.

#### Signatur

Agentenverweise und Workflows sind kritische Objekte, die nur autorisierte Personen übergeben dürfen.
Deshalb verlangt der Master Signaturen für die Objekte,
die er mit einem hinterlegten vertrauenswürdigen Schlüsselbund prüft. 

Der Agent prüft auf gleiche Weise die Signaturen der Workflows.

Die Signatur kann mit OpenPGP erstellt werden (getested mit GnuGP und Bouncy Castle).

Im Master wie im Agenten kann die Datei mit den vertrauenswürdigen Signaturschlüsseln wie folgt angeggeben werden.
```
jobscheduler.configuration.trusted-signature-keys {
  PGP = ${jobscheduler.config-directory}"/private/trusted-pgp-keys.asc"
}
```
Die Datei ```config/private/trusted-pgp-keys.asc``` enthält die ASCII-codierten öffentlichen PGP-Schlüsselbünde
mit den Schlüsseln der zugelassenen Signierer und 
kann zum Beispiel so erstellt werden:
```
gpg --export --armor --output=.../config/private/trusted-pgp-keys.asc
```
(Das Kommando exportiert alle Schlüsselbünde, was vielleicht nicht gewollt ist.)  

#### Signierte Objekte

Die Kommandos ReplaceRepo und UpdateRepo erwarten signierte Objekte in der Form
- ```string```: _"JSON-codiertes Objekt"_  // das JSON-codierte Objekt in einem String
- ```signature:```, ein Objekt mit den Feldern
  - ```TYPE: "PGP"```
  - ```signatureString:``` _"PGP-Signatur in ASCII"_

Das Feld ```string``` enthält Objekt, also einen Agentenverweis oder einen Workflow, einschließt der Felder
```TYPE```, ```path``` und ```versionId```, zum Beispiel
```
{
  "TYPE": "AgentRef",
  "path": "/MY-AGENT",
  "versionId": "MY-VERSION",
  "uri": "https://agent-host:4444"
}
```
oder
```
{
  "TYPE": "Workflow",
  "path": "/MY-WORKFLOW",
  "versionId": "MY-VERSION",
  "instructions": [ ... ],
  "jobs": [ ... }
}
```
Das JSON-codierte Objekt ist ein String, aus dessen UTF-8-Darstellung man die Signatur bildet.

#### Benutzerrecht

Der Benutzer, der die Kommandos übergibt, braucht das Recht ```UpdateRepo```, 
das in der Datei ```private/private.conf``` so gegeben werden kann:
```
jobscheduler.auth.users {
  USERNAME {
    password = "sha512:...",
    permissions = [ UpdateRepo ]
  }
}
``` 
Das ist nicht erforderlich bei einem ungeschützten Master (```public = true``` oder ```loopback-is-public = true```). 
Da kann jeder die Kommandos geben.
     
#### Kommando ReplaceRepo

Das Kommando ersetzt die vorhandenen Agentenverweise und Workflows.
Es hat die Felder
- ```TYPE: "ReplaceRepo"```
- ```versionId:``` _"versionId"_
- ```objects```: \[ _signiertes Objekt_, ... \]
  
Beispiel:  
```javascript
{
  "TYPE": "ReplaceRepo",
  "versionId": "1",
  "objects": [
    {
      "string": "{\"TYPE\": \"AgentRef\", ...}",
      "signature": {
        "TYPE": "PGP",
        "signatureString": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
      }
    }, {
      "string": "{\"TYPE\": \"Workflow\", ...}",
      "signature": {
        "TYPE": "PGP",
        "signatureString": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
      }
    }
  ]
}
```

#### Kommando UpdateRepo

Das Kommando ändert eine vorhandene Konfiguration. 
Es ist aufgebaut wie ReplaceRepo, mit folgenden Abweichungen:
- ```change``` enthält die zu setzenden Objekte. Jedes kann neu oder vorhanden sein. Im letzten Fall wird es ersetzt.
  Die Objekte werden wie beim Kommando ReplaceRepo angegeben.
- ```delete``` ein Array aus Objektpfaden mit dem Aufbau
  - ```TYPE```: ```"AgentRef"``` oder ```"Workflow"```
  - ```path```: _"pfad des objekts"_   

Beispiel:
```javascript
{
  "TYPE": "UpdateRepo",
  "versionId": "1",
  "change": [
    {
      "string": "{\"TYPE\": \"Workflow\", ...}",
      "signature": {
        "TYPE": "PGP",
        "signatureString": "-----BEGIN PGP SIGNATURE-----\n\n...\n-----END PGP SIGNATURE-----\n"
      }
    }
  ],
  "delete": [
    {
      "TYPE": "WorkflowPath",
      "path": "/WORKFLOW-A"
    }, {
      "TYPE": "AgentRefPath",
      "path": "/AGENT-A"
    }
  ]
}
````

#### Provider

Weil der Master die Konfiguration nur noch per Kommando entgegen nimmt, gibt es jetzt den Provider.
Das ist ein Dienst, der Verzeichnisse mit Konfiguration überwacht, Änderungen signiert und dem Master schickt.

Der Provider wird gestartet mit
```
java com.sos.jobscheduler.provider.ProviderMain -config-directory=.../config
```
Ein Startskript (bash) steht bereit unter ```bin/jobscheduler-provider```.

Die erwartete Verzeichnisstruktur ist ähnlich zu Master und Agent:
- _config_/```provider.conf``` _(nicht erforderlich)_
- _config_/```private/private.conf```
- _config_/```live/```
- _config_/```order-generators/```

Die Anmeldedaten für den Master und den privaten PGP-Schlüssel
gibt man in der Datei ```config/provider/private.conf``` an:
```
jobscheduler.provider {
  master.user = Provider
  master.password = "SECRET PROVIDER PASSWORD"
}

jobscheduler.provider.sign-with = PGP

jobscheduler.provider.private-signature-keys.PGP {
  key = ${jobscheduler.config-directory}"/private/private-pgp-key.asc"
  password = "PGP-PASSWORD"
}
```
Der Provider meldet sich beim Master als Benutzer 'Provider' mit dem Kennwort an.

Das ist nicht erforderlich bei einem ungeschützten Master (```public = true``` oder ```loopback-is-public = true```). 
Da kann jeder die Kommandos geben.

Die Datei ```config/private/private-pgp-key.asc``` enthält den privaten, ASCII-codierten PGP-Schlüssel, 
mit dem der Provider die Objekte signiert.

Der Provider selbst braucht bislang kein Datenverzeichnis. 
(Für die Protokolle wird man eins einrichten wollen.)

Der Provider überwacht die Verzeichnisse
- ```config/live``` mit Dateien für 
  - Agentenverweisen (_name_.agentref.json oder _name_.agentref.yaml) und 
  - Workflows (_name_.workflow.txt, _name_.workflow.json oder _name_.workflow.yaml)
  - Unterverzeichnisse können verwendet werden
- ```config/order-generators``` mit den Auftragsgeneratoren
  - _name_.order.xml (ähnlich JobScheduler 1)
  - Unterverzeichnisse können nicht verwendet werden.

Der Provider ist nicht für die Produktion geeignet (dafür müsste ich ihn stabiler machen).
Er hat keine Webservices.

Das Docker-Beispiel startet auch den Provider.

### Neue Anweisung 'retry'

Ein try-Block kann mit der neuen Anweisung ```retry``` wiederholt werden.
Die Anweisung kann direkt in einem catch-Block, oder in einer if-Anweisung in einem catch-Block gegeben werden.

Die if-Anweisung hat Zugriff auf ```catchCount```, das die Nummer des catch-Durchlaufs angibt.
```catchCount``` ist beim ersten Erreichen des catch-Blocks eins 
und erhöht sich bei jedem weiteren, 
wenn mit der Anweisung ```retry``` eine Wiederholung verlasst worden ist.
Er gibt also die Nummer des angefangenen Fehlers oder die Anzahl der mislungenen Versuche an.
```catchCount``` gilt im try-Block und im-Catch sowie in den if-Anweisungen darin.
An allen anderen Stellen ist der Wert null.  

Im folgenden Beispiel wird der Job FAIL zweimal ausgeführt.
```
try {                                              
  job FAIL;  
  job SKIPPED;    
} catch if (catchCount < 2) retry else fail;       
```

Wiederholung und Abbruch können separat behandelt werden:
```
try {
  ...                                              
} catch { 
  if (catchCount < 2) {
    job beforeRetry; 
    retry; 
  } else {
    job givingUp; 
    fail;
  }
}       
```


Try mit retry kann verschachtelt werden:
```
try {                                                  
  try {                                                
    job OKAY1;      
    try {                                              
      job FAIL1;  
      job OKAY;    
    } catch if (catchCount < 3) retry else fail;       
    job OKAY2;      
  } catch if (catchCount < 2) retry else fail;
} catch job OKAY3;  
```
Die Jobs im Beispiel werden in folgender Reihenfolge ausgeführt:

1. OKAY1
2. FAIL1
3. FAIL1, erste Wiederholung des inneren try-Blocks
4. FAIL1, zweite Wiederholung des inneren try-Blocks
5. OKAY1, erste Wiederholung des äußeren try-Blocks
6. FAIL1
7. FAIL1, erste Wiederholung des inneren try-Blocks
8. FAIL1, zweite Wiederholung des inneren try-Blockss
9. OKAY3  
                           
#### Verzögerung

```
try (retryDelays=[10, 20, 30]) {                                              
  job FAIL;  
} catch if (catchCount < 2) retry else fail;       
```

Die erste Wiederholung wird 10s, die zweite 20s und für alle weiteren 30s verzögert.

### Webservice POST /master/api/order akzeptiert Array

Statt eines einzelnen Auftrags können  
dem Webservice ```POST /master/api/order``` 
auch mehrere Aufträge in einem Array übergeben werden.
                           
                           
## 2018-12-13

### Webservice /master/api/fatEvent kann mit 429 "Too Many Requests" antworten

Das macht er, solange eine andere Anfrage auf demselben Webservice läuft. 

Die fetten Events arbeiten auf einem fetten Zustand (FatState), der erst aus dem Journal aufgebaut werden muss.
Das kann dauern, vielleicht Minuten. 
Wenn dem Client das zu lange dauert und die Anfrage wiederholt,
dann lehnt der Webservice die Anfrage mit 429 solange ab, wie noch die vorherige Anfrage läuft.
Damit schützt sich der JobScheduler vor Überlast.   

### Neue Anweisung "fail"

```
fail returnCode=7;
```
Verhält sich wie ein Job, der mit dem ReturnCode endet.

```
fail;
```
Wenn der Auftrag einen Fehler hat (der letzte Schritt fehlerhaft war),
dann wirkt es wie ```fail``` mit dem letzten ReturnCode.

Wenn der Auftrag keinen Fehler hat, wirkt es wie ```fail returnCode=x```. 
Der Wert x ist erstmal -1.
Entscheidend ist, dass der Auftrag einen Fehler hat: Outcome.Failed(ReturnCode(...)).


## 2018-12-07
### Fehlerbehandlung mit "try"
Wenn ein Auftrag in einer mit ```try { ... }``` einen Jobschritt fehlerhaft beendet,
dann setzt er mit den Anweisungen im folgenden Catch-Block fort.
```
define workflow {
  try {  
    execute executable="/SUCCEED", agent="AGENT";
    execute executable="/FAIL", agent="AGENT";
    execute executable="/NOT-EXECUTED", agent="AGENT";
  } catch {
    execute executable="/RECOVER", agent="AGENT";
  };
}
```
Einschränkungen
- Der Catch-Block muss erstmal wenigstens eine Anweisung enthalten.
- Fehler in einem Fork-Branch werden nicht abgefangen, 
  denn Fork verwirft die Ergebnisse der Kindaufträge.

## 2018-12-05

### JSON-Feld "scheduledAt" heißt jetzt "scheduledFor"

For better English. Betroffen sind die JSON-Klassen
- ```FreshOrder```, beim Einspeisen eines Auftrags über den Webservice api/order
- ```Order```, beim Lesen von Aufträgen über den Webservice api/order
- ```OrderAddedFat```, beim Lesen der fetten Events über den Webservice api/fatEvent  

Beispiel POST api/order
```
{
  "id": "MY-ORDER",
  "workflowPath": "/MY-WORKFLOW",
  "scheduledFor": 1543993583000
}
```

Beispiel GET api/order
```
{
  "id": "MY-ORDER",
  "workflowPosition": {
    "workflowId": {
      "path": "/MY-WORKFLOW",
      "versionId": "(initial)"
    },
    "position": [ 0 ]
  },
  "state": {
    "TYPE": "Fresh",
    "scheduledFor": 1543993583000
  }
}
```

Beispiel GET api/fatEvents
```
{
  "TYPE": "OrderAddedFat",
  "workflowPosition": {
    "workflowId": {
      "path": "/MY-WORKFLOW",
      "versionId": "(initial)"
    },
    "position": [ 0 ]
  },
  "scheduledFor": 1543993583000
}
```

### Json-Objekttyp "ForkJoin" heißt jetzt "Fork"

Die Anweisung "fork" heißt in JSON jetzt einfach "Fork" statt bisher "ForkJoin".

Zum Beispiel
```
{
  "TYPE": "Fork",
  "branches": [...]
}
```

### Neues Kommando CancelOrder

Zur Stornierung von Aufträgen.
Das Kommando antwortet mit ```{ "TYPE": "Accepted" }```, wenn die OrderId bekannt ist, andernfalls mit einem Fehler.
Das Kommando vermerkt den Stornierungswusch. 
Die Stornierung selbst ist asynchron. 
Der Master verfolgt dazu den Auftrag bis auf den Agenten, wo er storniert wird. Anschließend wird er zurückgeholt und gelöscht.

Storniert werden können Aufträge
- die kein Kindauftrag sind (Fork)
- die noch nicht gestartet sind (erster Job noch nicht gestartet und keine Fork-Anweisung am Anfang des Workflow)
- optional mit dem Parameter ````mode``` 
  - ```"mode": { "TYPE": "NotStarted" }``` nur Aufträge, die noch nicht gestartet sind (voreingestellt) 
  - ```"mode": { "TYPE": "FreshOrStarted" }``` auch Aufträge, die gestartet sind und 

CancelOrder ist wirkungslos bei Aufträgen, die das Ende des Workflows erreicht haben.
Diese Aufträge beenden sich normal.

Ein Auftrag wird nur storniert, wenn er im Zustand Ready ist, zum Beispiel nach einem Fork oder einer Job-Ausführung.
In anderen Fällen wird der Stornierungswunsch vorgemerkt.                                             

**JSON**
```
{
  "TYPE": "CancelOrder",
  "orderId": "MY-ORDER-ID",
  "mode": {
    "TYPE": "FreshOrStarted"
  }
}
```

Der Master nimmt Kommandos über den Webservice ```POST /master/api/command``` entgegen.

### Neues Kommando Batch

Erlaubt die Übergabe von mehreren Kommandos, die der Master unabhängig voneinander und ungeordnet ausführt.

Beispiel für zwei CancelOrder:
```
{
  "TYPE": "Batch",
  "commands": [
    {
      "TYPE": "CancelOrder",
      "orderId": "UNKNOWN-ORDER"
    }, {
      "TYPE": "CancelOrder",
      "orderId": "ANOTHER-ORDER"
    }
  ]
}
```

Die Antwort enthält das Array ```responses```.
Das Array enthält für jedes Kommando ein Objekt mit folgendem Inhalt.
- Wenn ein Problem auftretreten ist
  - ```"TYPE": "Problem"```
  - ```"code": "(fehlercode)"```, optional (einziger Fehlercode bislang: ```"UnknownOrder"```)
  - ```"message": "(fehlertext)"```
- Wenn kein Problem aufgetreten ist
  - ```"TYPE": "Accepted"```, oder abhängig vom Kommando ein anderer Typ, aber nicht "Problem".  
  
JSON auf das obige Batch-Kommando, wenn der Auftrag UNKNOWN-ORDER nicht bekannt ist.
 
```
{
  "TYPE": "BatchResponse",
  "responses": [
    {
      "TYPE": "Problem",
      "code": "UnknownOrder",
      "arguments": [
        "orderId": "UNKNOWN"
      },
      "message": "Unknown OrderId 'UNKNOWN-ORDER'"
    }, {
      "TYPE": "Accepted"
    }
  ]
}
```

Wenn der Master schob bei Annahme des Kommandos erkennt, dass der Auftrag nicht storniert werden kann,
dann antwortet er mit einer Fehlermeldung (```"TYPE": "Problem"```). 


## 2018-10-20

### Geänderte Workflow-Syntax 

Ein Workflow wird jetzt definiert mit ```define workflow { ... }```.
Zum Beispiel:

```
define workflow {
  job A;
  ...
}
```

Jobs werden nicht mehr am Agenten, sondern im Workflow definiert:

```
define workflow {
  job A, arguments={"hello": "Hello!"};
  job B;
  
  define job A {
    execute executable="/test-A", agent="agent-1";
  }
  
  define job B {
    execute executable="/test-B", agent="agent-2", taskLimit=30, arguments={"key": "value", "hello": "გამარჯობა!"};
  }
}
```

### Neue Workflow-Anweisungen

- ```job``` gibt den Namen eines Jobs an, der im Workflow mit ```define job``` definiert ist.
- ```define job``` definiert einen Job mit genau einer ```execute```-Anweisung.
- ```execute``` führt ein Programm _(executable)_ des Agenten aus. Die Anweisung hat die Parameter
  - executable=_ExecutablePath_ – Der Pfad des Programms muss mit einem Schrägstrich beginnen und darf weder die Verzeichnisse ".." noch "." enthalten.
  - agent=_AgentPath_
  - arguments=_JsonObject_ – Parameter für das Programm, notiert wie ein JSON-Objekt mit String-Werten. 
    Der Prozess erhält die Parameter als großgeschriebene Umgebungsvariablen mit dem Präfix "SCHEDULER_PARAM_" (kompatibel zu JobScheduler 1), 
    außerdem mit Vorrang die Auftragsvariablen.   

Die Anweisung ```executable``` kann auch direkt im Workflow verwendet werden:
```
define workflow {
  execute executable="/test-C", agent="agent-1";
}
```

  
### Konfiguration im Agenten

Statt der Jobs erwartet der Agent bloß die zugelassenen Programme in seinem Konfigurationsverzeichnis ```config/executables```. 
Unterverzeichnisse und symbolische Links sind möglich.
Unter Unix muss das _executable_-Bit gesetzt sein (chmod +x).
