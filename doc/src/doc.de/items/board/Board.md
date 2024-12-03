# Board und Notice

JS7 kennt benannte schwarze Bretter, an denen Notizen haften.
* Ein Auftrag kann für einen anderen Auftrag (oder sich selbst) eine Notiz hinterlassen.
* Eine Notiz hat eine `NoticeId`, die für das jeweilige schwarze Brett gilt.
* Ein Auftrag kann eine Notiz an ein schwarzes Brett heften
* und dabei mit einer Termin versehen, ab dem JS7 die Notiz entfernen kann.
* Ein Auftrag kann auf eine Notiz mit einer bestimmten `NoticeId` warten.

## Board
Ein `Board` ist ein `UnsignedSimpleItem` (nicht signiert, nicht versioniert) und hat die Parameter
* `path: BoardPath`
* `toNotice: Expression`
  * Ausdruck, der für die Anweisung `PostNotices` eine `NoticeId` liefert.
* `endOfLife: Expression`
  Ausdruck, der für die Anweisung `PostNotices` den Zeitpunkt liefert, bis wann die Notiz gelten soll, ausgedrückt als Anzahl Millisekunden seit 1970-01-01, 0 Uhr, UTC. Dann wird JS7 die Notiz löschen.
* `expectingOrderToNoticeId`
  * Ausdruck, der für die Anweisung `ExpectNotices` eine `NoticeId` liefert.

Die Ausdrücke haben Zugriff auf den Auftrag (`$js7OrderId`, Auftragsvariablen) und die Uhr (`$js7EpochMilli`).

JSON-Beispiel, das die NoticeId aus dem Tagesplantag einer OrderId "#yyyy-mm-dd#..." errechnet:
```json
{
  "TYPE": "Board",
  "path": "MY-BOARD",
  "toNotice": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')",
  "endOfLife": "$js7EpochMilli + 24 * 3600 * 1000",
  "expectingOrderToNoticeId": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1')"
}
```

## Anweisung PostNotices

Platziert eine Notiz auf dem genannten schwarzen Brettern. Vielleicht vorhandene Notizen (mit derselben NoticeId) wird ersetzt.

`noticeId` und `endOfLife` der Notiz werden mit den im Board angegebenen Ausdrücken errechnet, sodass bei der Anweisung kein weiterer Parameter erforderlich ist.

JSON
```json
{
  "TYPE": "PostNotices",
  "boardPath": [
    "A-BOARD",
    "B-BOARD"
  ]
}
```


## Anweisung ExpectNotices

Die erwarteten schwarzen Bretter sind als Ausdruck anzugegeben. Die kleine Ausdrucksprache hierfür kennt konstante Strings (für  BoardPath) und die Operatoren `&&` und `||'. `&&` bindet stärker.

JSON
```json
{
  "TYPE": "ExpectNotices",
  "boardPaths": "'A-BOARD' || 'B-BOARD' && 'C-BOARD'"
}
```

Die `noticeId` der Notiz wird mit den im Board angegebenen Ausdruck errechnet, so dass bei der Anweisung kein weiterer Parameter erforderlich ist.

Wenn die verlangten Notizen nicht auf allen schwarzen Brettern ist, wartet die Anweisung, bis andere Aufträge sie mit `postNotices` angeheftet haben.
