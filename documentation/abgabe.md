# WadLoader-Projekt: Detaillierte Problemstellung und Lösung

## Problemstellung

WAD-Dateien (Where's All the Data) sind Container für Ressourcen, die in der Doom-Engine genutzt werden. Diese Dateien sind essenziell für die Anpassung und Erweiterung des Spiels Doom. Die vorliegende Problemstellung dreht sich um die Entwicklung einer Webanwendung namens WadLoader. Diese ermöglicht es, WAD-Dateien hochzuladen, zu organisieren und herunterzuladen. Oft werden mehrere WADs zusammen verwendet, um verschiedene Szenarien oder Mods zu erstellen. Das Hauptziel der Anwendung ist es, eine benutzerfreundliche Oberfläche bereitzustellen, um diese Dateien zu verwalten und sie in Form von Modpacks (auch WAD-Packs) zu organisieren.

## Use Cases (Anwendungsfälle)

### Benutzer erstellen (Create User UC)

```mermaid
graph
A(Open WadLoader3) --> B(Enter Name and Password for new User)
B --> C(Create User)
```

### Anmeldung (Login UC)

```mermaid
graph
A(Login) --> B(Upload Wad from\nlocal FileSystem)
B --> C(Edit Name)
```

### WAD hochladen (Upload WAD UC)

```mermaid
graph
A(Login) --> B(Upload .wad-File)
B --> C(Enter Wad-Name)
C --> D(Wad File appears in the UI)
```

### WAD-Paket erstellen (Create WAD Pack UC)

```mermaid
graph
A(Upload WAD UC) --> B(Click 'Create WAD-pack')
B --> Z(Search .wad-Files)
B --> C(Select .wad-Files to add)
C --> Z
Z --> C
C --> X(Change order of Wads) 
C --> D(Name .wad-pack) 
X --> D
D --> Y(Select IWAD to use)
Y --> E(Get link to WAD-pack)
```

### WAD-Pakete herunterladen (Download WAD Packs UC)

```mermaid
graph
A(Get a link to a WAD-Pack) --> B(Click 'Download')
B --> C(Get a zip file)
```

### WADs durchsuchen (Browse Wads)

```mermaid
graph
A(Open WadLoader) --> B(Select 'Wad Search')
B --> C(Enter Search String)
```

### WAD-Pakete durchsuchen (Browse Wad-Packs)

```mermaid
graph
A(Open WadLoader) --> B(Select 'Wad-pack Search')
B --> C(Enter Search String)
```

## Muss-/Kann-Kriterien

Die Anwendung muss folgende Kriterien erfüllen:

- Benutzerregistrierung und -authentifizierung
- Hochladen und Verwalten von WAD-Dateien
- Erstellen, Bearbeiten und Herunterladen von WAD-Paketen
- Durchsuchen und Filtern von WAD-Dateien und -Paketen
- Integration einer geeigneten Datenbank für Persistenz

Kann-Kriterien umfassen:

- Unterstützung für mehrere Benutzerrollen (Administrator, Standardbenutzer)
- Erweiterte Such- und Filteroptionen für WAD-Dateien
- Integration von OAuth für externe Anmeldungsoptionen
- Starten von WAD-Packs aus dem Browser heraus

### Umgesetzte Kriterien

Es wurden alle Muss-Kriterien implementiert. Benutzer können sich beim WadLoader registrieren und anmelden. WADs können hochgeladen und mit anderen Nutzern geteilt werden. Es ist möglich, WAD-Packs zu erstellen, zu bearbeiten und herunterzuladen. Alle WAD-Packs können mittels einer Suchfunktion gefiltert werden. Alle Änderungen werden durch eine Datenbank persistiert, somit bleiben diese auch nach einem Neustart oder Absturz des Servers erhalten.

Zusätzlich wurden auch die meisten Kann-Kriterien erfüllt. Such- und Filterfunktionalität steht auch für WADs zur Verfügung. Das Anmelden mittels OAuth ist möglich. Zudem können auch bereits existierende Accounts von externen OAuth-Anbietern (Google, Github, etc.) zum Anmelden und Registrieren genutzt werden.

## Technologieauswahl

Im Folgenden wird der Einsatz der genutzten Technologien begründet.

### Begründung H2

Eine H2-Datenbank lässt sich dank der Integration in Spring Boot mit minimalem Aufwand einsetzen. Da das zu persistierende Datenmodell klein ist (zwei Entitäten), ist H2 vorerst ausreichend. Zusätzlich haben wir bereits umfangreiche Erfahrung im Umgang mit H2, was die Implementierung und Wartung erleichtert. Bei Bedarf kann die Datenbank einfach durch eine andere ersetzt werden, was uns Flexibilität für zukünftige Erweiterungen bietet. H2 ist zudem ideal für Entwicklungs- und Testumgebungen, da sie schnell und ressourcenschonend ist. Eine In-Memory-Datenbank vereinfacht das Aufsetzen der Laufzeitumgebung, da keine eigene Datenbank installiert oder anderweitig (z.B. über Docker) bereitgestellt werden muss. Durch eine saubere Trennung der Persistenzschicht von der Domäne kann die H2-Datenbank ohne Änderungen am Kern der Anwendung ausgetauscht werden.

### Begründung Spring Boot

Spring Boot ist für Java-Webanwendungen eine beliebte Wahl, da es plattformunabhängig ist und das Bereitstellen unter verschiedenen Betriebssystemen erleichtert. Unsere Teammitglieder verfügen über fundierte Kenntnisse in Spring Boot, und wir möchten diese weiter ausbauen. In vielen Unternehmen, in denen wir tätig sind, wird Spring Boot aufgrund seiner Robustheit und des umfangreichen Ökosystems verwendet. Dies verschafft uns berufliche Vorteile. Darüber hinaus ermöglicht Spring Boot eine schnelle Entwicklung und Bereitstellung von Anwendungen durch seine konventionsbasierten Konfigurationsmöglichkeiten. Ein breites Spektrum an Spring-Integrationen ermöglicht die nahtlose Verwendung vieler anderer Frameworks und Bibliotheken. Die vergleichsweise langsamen Startzeiten von Spring Boot sind bei Anwendungen dieser Größe noch im einstelligen Sekundenbereich. Das Ausführen des Servers ist also auch auf älteren Geräten möglich.

### Begründung React/TS

React mit TypeScript ermöglicht die Entwicklung moderner Webanwendungen mit Material UI für ein konsistentes Benutzererlebnis. Wir haben bereits umfangreiche Erfahrung in der Entwicklung mit React und TypeScript. React ist in vielen Unternehmen weit verbreitet und verbessert unsere beruflichen Fähigkeiten. TypeScript bietet statische Typisierung, die die Codequalität und Wartbarkeit erhöht. Die Verwendung von Material UI sorgt für ein einheitliches und ansprechendes Design. Diese Kombination ermöglicht uns eine effiziente und fehlerarme Entwicklung.

### Begründung Haskell

Haskell wird in diesem Projekt für den Client Handler verwendet. Dies geschieht hauptsächlich aus Experimentierfreudigkeit heraus. Es soll getestet werden, wie viel Aufwand notwendig ist, um stark auf IO-basierende Funktionalität in Haskell abzubilden. Haskell bietet sich für den Client Handler an, da es plattformunabhängig ist und ohne zusätzliche Interpreter nativ ausführbar ist. Darüber hinaus besteht im Team bereits umfangreiches Vorwissen im Bereich Haskell, was die Entwicklung erleichtert. Die Nutzung von Haskell dient zudem als Prüfungsvorbereitung für die Klausur in der Vorlesung Programmierparadigmen. Haskells starke Typensicherheit und funktionale Programmierparadigmen ermöglichen eine saubere und wartbare Codebasis, was insbesondere bei komplexen Anwendungen wie dem WadLoader von Vorteil ist. Die Nutzung von Java oder Python würde beispielsweise die Installation einer Java- bzw. Python-Laufzeit erfordern. Alternativ könnte die entsprechende Laufzeitumgebung auch mit dem Handler ausgeliefert werden, was den Client Handler jedoch aufblähen würde.

### Begründung Auth0

Authentifizierung und Autorisierung ist ein sehr komplexes und sensibles Gebiet. Das Implementieren eines OAuth2-Flows ist nicht trivial. Deswegen wird Auth0, ein Anbieter für Authentication und Authorization als Service, genutzt. Dadurch müssen vom WadLoader keine Userdaten verwaltet werden. Das Anmelden über bereits bestehende Konten (z.B. GitHub) ist möglich. Die Nutzung von Auth0 reduziert also den Entwicklungsaufwand und bringt zusätzlich viele Vorteile.

# Architekturmodelle

## Spring-Boot-Backend

![Spring-Boot-Backend](https://hackmd.io/_uploads/rkhSzEYI0.png)

Das Backend ist nach der Onion-Architektur strukturiert, um eine klare Trennung von Geschäftslogik und Infrastruktur zu gewährleisten. Diese Architektur fördert eine modulare und erweiterbare Codebasis, indem sie Schichten definiert, die sich um das zentrale Domänenmodell gruppieren.

### Infrastruktur

Die Infrastrukturschicht bildet die äußerste Schicht und enthält Implementierungen für datenbankbezogene Operationen, Dateimanagement und externe Schnittstellen. Beispielsweise umfasst sie die Implementierungen für das Zippen von Dateien oder das Verwalten des Dateisystems auf dem Server.

### API

In der API-Schicht werden die Controller definiert, die die REST-Schnittstellen zur Kommunikation mit dem Frontend und dem Client-Handler bereitstellen. Diese Schicht verwaltet auch die Umwandlung von Domain-Objekten in DTOs (Data Transfer Objects), die über das Netzwerk übertragen werden.

### Domain/Core

Die Domänenschicht bildet das Herzstück der Anwendung und enthält die Geschäftslogik sowie die zentralen Entitäten wie Wads und WadPacks. Hier werden die Kernoperationen definiert, die die Anwendungslogik implementieren und auf die Infrastrukturschicht zugreifen.

### Abstraktion

Die Abstraktionsschicht bietet allgemeine Schnittstellen und abstrakte Klassen, die die Kommunikation zwischen der Domänenschicht und der Infrastrukturschicht ermöglichen. Dies fördert die Austauschbarkeit und Testbarkeit der Infrastruktur

komponenten.

## Datenbankmodell

Die H2-Datenbank wird genutzt, um die Daten der WAD-Dateien und der WAD-Packs zu speichern. Eine einfache Tabellenstruktur wird verwendet, um die Beziehungen zwischen den verschiedenen Entitäten zu verwalten. Durch die Nutzung der JPA (Java Persistence API) wird die Datenpersistenz abstrahiert, was die Wartung und Weiterentwicklung erleichtert.

## React/TypeScript-Frontend

![React-TS-Frontend](https://hackmd.io/_uploads/B1oSOtY2A.png)

### Benutzeroberfläche

Das Frontend nutzt React und TypeScript, um eine reaktive und interaktive Benutzeroberfläche zu bieten. Material UI wird zur Gestaltung des Layouts und der Komponenten verwendet, um eine konsistente und ansprechende Benutzererfahrung zu gewährleisten.

### Zustand

Der Zustand der Anwendung wird mithilfe von React's Context API und Hooks verwaltet. Dies ermöglicht eine einfache und skalierbare Verwaltung des Anwendungszustands, insbesondere für die Authentifizierung und die Verwaltung der WAD-Dateien und WAD-Packs.

### Kommunikation

Die Kommunikation mit dem Backend erfolgt über REST-APIs, die mit Axios aufgerufen werden. Die erhaltenen Daten werden in der Anwendung verarbeitet und dargestellt, um dem Benutzer eine nahtlose Interaktion zu ermöglichen.

### Authentifizierung

Die Authentifizierung wird durch die Integration von Auth0 realisiert. Dies ermöglicht eine sichere und einfache Anmeldung und Verwaltung von Benutzern, ohne dass sensible Daten direkt in der Anwendung gespeichert werden müssen.

## Haskell Client-Handler

### Zweck und Funktion

Der Client-Handler ist ein in Haskell geschriebenes Programm, das als Vermittler zwischen dem Benutzer und der Webanwendung dient. Es ermöglicht das Hochladen von WAD-Dateien und das Erstellen von WAD-Packs über eine Kommandozeilenschnittstelle.

### Integration und Kommunikation

Der Client-Handler kommuniziert über REST-APIs mit dem Spring-Boot-Backend und ermöglicht so das Hochladen und Verwalten von Dateien. Die Nutzung von Haskell ermöglicht eine starke Typensicherheit und eine funktionale Programmierweise, die eine saubere und wartbare Codebasis gewährleistet.

### Funktionsweise

WADs werden als unveränderliche Daten betrachtet. Dies hat den Vorteil, dass es nicht zu Inkonsistenzen kommt, wenn einzelne WADs gelöscht oder aktualisiert werden. Das Löschen von WADs würde die Konsistenz von WAD-Packs zerstören, die den WAD genutzt haben. Beim Aktualisieren von WADs kann es, wie beim Updaten von Software, zu Fehlern und Inkonsistenzen kommen.

WAD-Packs können beliebig bearbeitet werden. Deswegen werden beim Herunterladen und Starten eines WAD-Packs das Vorhandensein aller benötigten WADs geprüft und ein neues Startscript heruntergeladen.

## Client Handler

Die folgende Grafik gibt eine grobe Übersicht über die Abläufe im Client Handler.

![Haskell Handler Diagramm](https://hackmd.io/_uploads/B11CZioER.png)

Aus den Parametern des Programmes ergibt sich eine **Action**. Diese ist eine Anweisung, welche WADs und welches Start-Skript (für ein WAD-Pack) herunterzuladen sind. Zudem enthält die Action die URL des Servers, welcher zum Herunterladen der WADs verwendet werden soll.

Aus der Liste der benötigten WADs werden diejenigen ermittelt, welche dem Client noch nicht lokal zur Verfügung stehen.

Die Liste der noch herunterzuladenden WADs wird mit der ID des WAD-Packs an die URL des Servers weitergeleitet.

Der Server antwortet auf diese Anfrage mit einer .zip Datei. Diese enthält alle angeforderten WADs und eine .cmd Datei, welche das WAD-Pack startet. Diese .cmd Datei wird zum Ausführen eines einzelnen WAD-Packs genutzt.

Bei der Entwicklung des Client Handlers wurden Best Practices der Funktionalen Programmierung genutzt. Dazu gehört beispielsweise das Schieben von IO Operationen an den Rand der Anwendung. Die meisten Operationen des Kerns der Anwendung sind also pure.

# Screenshots und Zustände

## Create WAD

Hier ist ein Screenshot der "Create WAD"-Seite im WadLoader-Projekt. Diese Seite ermöglicht es dem Benutzer, ein neues WAD-File hochzuladen und einen Namen und eine Beschreibung dafür einzugeben.

![image](https://hackmd.io/_uploads/BJCP_ad8R.png)

## Create WAD Pack

Dies ist ein Beispiel für die "Create WAD Pack"-Seite. Der Benutzer kann ausgewählte WAD-Files zu einem Paket zusammenstellen, die Reihenfolge ändern und dem Paket einen Namen und eine Beschreibung geben.

![image](https://hackmd.io/_uploads/ByzruadU0.png)

## WAD List

Hier ist die Ansicht der "WAD List", die dem Benutzer ermöglicht, nach WAD-Dateien zu suchen und sie herunterzuladen.

![image](https://hackmd.io/_uploads/SkAfupuI0.png)

# Anwendung ausführen

## Setup

Notwendige Einrichtung, um das Programm auszuführen. Das Festlegen von localhost:3000 ist nur erforderlich, wenn das Frontend von Vite bereitgestellt werden soll. Dies bietet sich hauptsächlich während der Entwicklung an.

1. Erstellen eines Authentifizierungsprojekts bei auth0
    * Erstellen eines kostenlosen Kontos unter https://auth0.com
    * Erstellen einer neuen "Regular Web Application"
    * Hinzufügen von "Allowed Callback URLs": http://localhost:8080/login/oauth2/code/okta
    * Hinzufügen von "Allowed Logout URLs": http://localhost:3000, http://localhost:8080
    * Hinzufügen von "Allowed Web Origins": http://localhost:3000, http://localhost:8080
    * Aktivieren von "Allow Cross-Origin Authentication" und Hinzufügen von Ursprüngen: "http://localhost:3000, http://localhost:8080"
2. Erstellen der Datei src/main/resources/application-local.properties
3. Hinzufügen von Einträgen für (okta.XXX-Werte stammen aus dem zuvor erstellten auth0-Projekt):
    * spring.datasource.username
    * spring.datasource.password
    * okta.oauth2.issuer
    * okta.oauth2.client-secret
    * okta.oauth2.client-id

## Anwendung bauen

Erforderliche Werkzeuge:
* Java: 21
* Haskell: GHC2021 (\*)
* Node: 20 (niedrigere Versionen sollten auch funktionieren)
* npm (js/ts-Build-Tool)
* yalc (Manager für lokale npm-Pakete, kann über npm installiert werden)
* Maven (Java-Build-Tool)
* Cabal (Haskell-Build-Tool, empfohlene Installation über ghcup, \*)

\* nur notwendig, wenn Änderungen am Haskell Code vorliegen. Die folgenden Schritte helfen Ihnen, eine ausführbare Jar-Datei zu erstellen. Alle Schritte setzen das Stammverzeichnis des Repositories als Startverzeichnis voraus.

### API bauen

1. cd ./api-generator
2. npm install
3. npm run generate
4. npm run update

### Haskell-Client bauen (kann ausgelassen werden, wenn keine Änderungen am Haskell-Code vorgenommen wurden)

1. cd ./local-client
2. cabal build
3. cp ./dist-newstyle/build/x86_64-windows/ghc-9.4.8/local-client-0.1.0.0/x/local-client/build/local-client/local-client.exe ../wadloader3/src/main/resources/local-client.exe

### Backend und Frontend bauen

1. cd ./wadloader3
2. yalc add wadloader3-api
3. npm install
4. npm run build
5. mvn package

## Server starten

Starten Sie die Anwendung ohne zusätzliche Parameter.

## Anwendung als Benutzer einrichten

1. Anmeldung
2. Herunterladen des Setup-Skripts (durch Klicken auf die Schaltfläche zum Herunterladen oder Aufrufen von /download/setup)
3. Ausführen des Setup-Skripts mit Administratorrechten (es kann sich auch selbst erhöhen)

Optional (wenn Sie diese nicht setzen, werden Sie jedes Mal nach ihren Werten gefragt, wenn Sie ein WadPack starten):
4. Setzen Sie %GZDOOM_PATH% auf den Pfad zu Ihrer gzdoom.exe
5. Setzen Sie %IWAD_PATH% auf den Pfad des IWAD, den Sie zum Starten Ihrer WadPacks verwenden möchten

# Zukünftige Erweiterungen

1. Unterstützung für zusätzliche Spiel-Engines neben der Doom-Engine.
2. Erweiterte Benutzerrollen und Berechtigungssysteme.
3. Unterstützung für Cloud-Speicher zur Verwaltung von WAD-Dateien.
4. Verbesserte Such- und Filterfunktionen.
5. Integration von Social Features wie Bewertungen und Kommentare zu WADs.
6. Mobile App zur Verwaltung von WAD-Dateien unterwegs.

# Sicherheit

1. Implementierung von Sicherheitsmaßnahmen wie SSL/TLS für die Datenübertragung.
2. Regelmäßige Sicherheitsupdates und Patches.
3. Überprüfung und Validierung von hochgeladenen WAD-Dateien.
4. Schutz vor Brute-Force-Angriffen und anderen bösartigen Aktivitäten.

# Leistungsoptimierungen

1. Caching von häufig verwendeten Daten.
2. Optimierung der Datenbankabfragen.
3. Lastverteilung für den Serverbetrieb.
4. Überwachung und Profiling der Anwendung zur Identifizierung von Engpässen.

# Deployment

1. Einsatz von CI/CD-Pipelines für automatisierte Builds und Tests.
2. Verwendung von Docker für eine konsistente und portable Deployment-Umgebung.
3. Bereitstellung in einer Cloud-Umgebung für Skalierbarkeit und Zuverlässigkeit.
4. Regelmäßige Back

ups und Wiederherstellungstests.

# Dokumentation und Benutzerhandbuch

1. Erstellung eines umfassenden Benutzerhandbuchs mit Schritt-für-Schritt-Anleitungen.
2. Bereitstellung von API-Dokumentationen für Entwickler.
3. Erstellen von Video-Tutorials und Demos zur Veranschaulichung der wichtigsten Funktionen.
4. Einrichten eines Support-Systems für Benutzerfragen und -probleme.

# Fazit

Das WadLoader-Projekt stellt eine umfassende Lösung zur Verwaltung und Ausführung von WAD-Dateien dar. Durch die Nutzung moderner Technologien und Best Practices wird eine hohe Benutzerfreundlichkeit und Wartbarkeit erreicht. Die flexible Architektur ermöglicht zukünftige Erweiterungen und Anpassungen, um den sich ändernden Anforderungen gerecht zu werden. Durch die Implementierung robuster Sicherheitsmaßnahmen und Leistungsoptimierungen wird sichergestellt, dass die Anwendung sowohl sicher als auch effizient ist. Insgesamt bietet der WadLoader eine robuste Plattform für Enthusiasten und Entwickler, die das volle Potenzial von WAD-Dateien ausschöpfen möchten.
