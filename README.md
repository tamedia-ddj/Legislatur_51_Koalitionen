# Mehrheitsfähige Koalitionen im Nationalrat
Mit den Wahlen für die 51 Legislatur hat sich sowohl der Anteil der Links-grüner Parteien, als auch der Frauenanteil in allen Parteien deutlich erhöht.
Das Ziel dieser Analyse ist es nun, die Auswirkungen dieser Veränderungen zu erkennen. Grundlage dafür waren alle Abstimmungen im Nationalrat in der Legislatur 50 und 51 (bis 18.12.2020), welche über die [offizielle API des Schweizer Parlamants](https://pragmatiqa.com/xodata/odatadir.html) verfügbar sind.

Die Analyse besteht aus drei Teilen:

1. Abfragen der benötigten Daten aus der offiziellen API des Parlaments (R-Script [1_Request_API_.R](1_Request_API_.R))
2. Definition und Analyse der Koalitionen  (R-Script [2_Analyse.R](2_Analyse.R))
3. Darstellung und Zusammenfassung der Koalitionen [(GoogleSheet)](https://docs.google.com/spreadsheets/d/1DUT3gIYoZhuRYYhOSLtZ-6Gr6nIuJQsc37o8aPXWKmU/edit?usp=sharing)

Die Erkenntnisse aus der Analyse werden am 27.12.2020 [in der Sonntagszeitung publiziert](https://www.tagesanzeiger.ch/sonntagszeitung "Artikel in der Sonntagszeitung") publiziert.

## 1. Abfrage der Daten aus der API
--> R-Script [1_Request_API_.R](1_Request_API_.R)

In einer ersten Abfrage wurde die Liste aller Abstimmungen im Nationalrat der betrachteten Legislaturperioden (50 & 51) von der API angefragt.
(--> https://ws.parlament.ch/odata.svc/Vote)

In einem zweiten Schritt wurden die Voten aller Parlamentarier zu diesen Abstimmungen von der API abgefragt.
(--> https://ws.parlament.ch/odata.svc/Voting)

Die Resultate beider Anfragen werden als .csv Datei pro betrachteter Legislatur exportiert:
- Ids_50.csv / Ids_51.csv  (--> Liste mit den Ids Schluss- und Gesamtabstimmungen)
- Ids_50_alle.csv / Ids_51_alle.csv  (--> Liste mit den Ids aller Abstimmungen)

- votes_50.csv / votes_51.csv  (--> Liste mit Voten von allen Parlamentariern zu Schluss- und Gesamtabstimmungen) --> Zipfile aus Platzgründen
- votes_50_alle.csv / votes_51_alle.csv  (--> Liste mit Voten von allen Parlamentariern zu allen Abstimmungen) --> Zipfile aus Platzgründen

Aus platzgründen sind diese csv files als Zip-Archiv hier hochgeladen.

## 2. Analyse der Koalitionen
--> R-Script [2_Analyse.R](2_Analyse.R)

Die Dateien votes_XX.csv aus dem vorhergehenden Schritt werden eingelesen und weiter aufbereitet. (Dieses Script kann ausgeführt werden, ohne das vorhergehende Request-API script auszuführen, da alle relevanten Daten in diesem Github repo enthalten sind (im Zip-Archiv).

Es wurde untersucht, in welchen Koalitionen Mehrheitsverhältnisse zustande kamen. Alle Analysen wurden sowohl mit allen Abstimmungen gemacht, als auch nur mit den Schluss- und Gesamtabstimmungen.

Damit eine Mehrheit einer bestimmten Koalition zugeordnet werden kann, wurde folgendes Kriterium angewendet: Die Koalitionspartner müssen jeweils mit mindestens 66% der anwesenden Fraktionsmitglieder zur Mehrheit beitragen. Die Koalitionsgegner dürfen zu höchstens 40% zur Mehrheit beitragen. Weitere Fraktionen, die nicht explizit Koalitionspartner oder oder Koalitionsgegner sind, dürfen mit 40% - 66 % zur Mehrheit beitragen.

Insgesamt wurden 35 verschiedene Kombinationen definiert, die anschliessend zu den unten aufgeführten Haupt-Koalitionen zusammengefasst wurden. Dabei wurde darauf geachtet, dass sich die verschiedenen Kombinationen nicht überschneiden, dass also jede Abstimmung nur einer Koalition zugewiesen wird.

Es wurden auch Mehrheiten berücksichtigt, welche zu einer Ablehnung einer Vorlage geführt haben. Die Analyse wurde wiederum getrennt für alle Abstimmungen, sowie nur für die Schluss- und Gesamtabstimmungen gemacht.

Eine Fraktion wird als Teil einer Koalition gesehen, wenn sie mit mindestens 66% der Fraktionsstimmen an der Mehrheit beteiligt ist. Im Gegenzug wird Sie als Koalitionsgegner gesehen, wenn sie mit weniger als 40% der Fraktionsstimmen an der Mehrheit beteiligt ist.

Alle untersuchten Koalitions-Kombination wurden zu den untenstehenden 7 Koalitionen zusammengefasst. Eine genau Auflistung und Definition aller 35 definierten Koalitionen finden sich im GoogleSheet [(3_Resultate_Koalitionen)](https://docs.google.com/spreadsheets/d/1DUT3gIYoZhuRYYhOSLtZ-6Gr6nIuJQsc37o8aPXWKmU/edit?usp=sharing), bzw. natürlich im R-Code.

**Links:**   
  SP oder SP und Grüne min. 66% der Fraktionstimmen   
  SVP und FDP max. 40% der Fraktionstimmen
  Mitteparteien weniger als 66% der Fraktionsstimmen

**Mitte-Links:**   
  SP oder Grüne min. 66% der Fraktionstimmen   
  SVP und FDP max. 40% der Fraktionstimmen
  Mindestens eine Mittepartei (Mitte, GLP) mehr als 66%
  
**Mitte:**   
  Mindestens zwei der Mitteparteien (Mitte, FDP, GLP) mit min. 66% der Fraktionstimmen   
  SP, Grüne und SVP max. 40% der Fraktionstimmen

**Mitte Rechts:**   
  SVP oder SVP und FDP min. 66% der Fraktionstimmen   
  Mindestens eine der Mitteparteien (Mitte, GLP) mit min. 66% der Fraktionstimmen   
  SP und Grüne max. 40% der Fraktionstimmen  

**Rechts:**   
  SVP oder SVP und FDP min. 66% der Fraktionstimmen 
  SP und Grüne max. 40% der Fraktionstimmen

**Grosse Koalitionen:**   
  Mindestens alle Bundesratsparteien mit min. 66% der Fraktionstimmen 
  Alle anderen mit nicht weniger 40% der Fraktionstimmen

**Alle gegen die SVP:**   
  SVP mit max. 40% der Fraktionstimmen
  Die anderen Bundesratsparteien mit min. 66% der Fraktionsstimmen
  GLP und Grüne mit min. 40% der Fraktionsstimmen

Weitere Koalitionen wie “Unheilige Allianzen” (SVP + SP), “Progressive Allianzen” (SP + FDP + Grüne + GLP) oder “Alle gegen die FDP” wurden ebenfalls untersucht, kommen jedoch nur sehr selten vor. Sie werden zusammen mit den nicht Kategorisierten Abstimmungen unter “Übrige Mehrheiten” zusammengefasst. 



## 3. Zusammenfassen der Koalitionen und Darstellung

--> [GoogleSheet](https://docs.google.com/spreadsheets/d/1sM7g3MWWxTKQn3FLKHDximCd5snhy9MDKIG9DksEqFc/edit?usp=sharing)
