---
title: Softwareprojekt Bericht - Entwicklung eines WadLoaders für Multiplayer Doom-Spiele
author: Lukas Hörnle, Maximilian Lincks
date: [Datum einfügen]
---

# Detaillierte Problemstellung

* Multiplayer Doom Spielen mit Mods ist schwierig weil:
    * Jeder Spieler benötigt alle Mods.
    * Die Mods müssen in der gleichen Reihenfolge installiert sein.
    * Es gibt keine standardisierte Lösung.
    * Clientseitige Software ist optional, was lokale Installationen überflüssig macht.

# Use Cases

* Archiv für .wad-Dateien (Upload/Download von .wads)
* Suche nach .wad-Dateien (nach Namen und optional nach Eigenschaften wie Pfaden)
* Anmeldung/Abmeldung (Login/Logout)
* Nutzerverwaltung
* Gruppierung mehrerer .wad-Dateien in WadPacks
    * Bearbeiten eigener Packs
    * Optionales Kopieren von Packs anderer Nutzer
* Download einzelner Dateien oder ganzer WadPacks
* Erstellung von Shell-Befehlen zum Starten von .wads
* Optionales Starten von .wads oder WadPacks über lokalen Handler

# Use Cases# Usecase Upload File

## Create User UC

```mermaid
graph
A(open WadLoader3) -->B(Enter Name and Password for new User)
    B --> C(Create User)
```

## Login UC

```mermaid
graph
A(Login) -->B(Upload Wad from\nlocal FileSystem)
    B --> C(Edit Name)
```

## Upload WAD UC

```mermaid
graph
A(Login) -->B(upload .wad-File)
    B --> C(Enter Wad-Name)
    C --> D(Wad File appears in the UI)
```

## create WAD Pack UC 

```mermaid
graph
A(Upload WAD UC) --> B(click ''create WAD-pack'')
    B --> Z(search .wad-Files)
    B --> C(Select .wad-Files to add)
    C --> Z
    Z --> C
    C --> X(Change order of wads) 
    C --> D(name .wad-pack) 
    X --> D
    D --> Y(select Iwad to use)
    Y --> E(get link to wadpack)
```

## Download WAD Packs UC 

```mermaid
graph
A(get a link to a WAD-Pack) --> B(click ''download'')
    B --> C(get a zipfile)
```

## Browse Wads

```mermaid
graph
A(Open WadLoader) --> B(select ''wad search'')
    B --> C(Enter Search String)
```

## Browse Wad-Packs

```mermaid
graph
A(Open WadLoader) --> B(select ''wad-pack search'')
    B --> C(Enter Search String)
```

# Muss-/Kann-kriterien

* Muss: Es muss ein Disclaimer geben, der darauf hinweist, dass nur Mods mit allen erforderlichen Rechten hochgeladen werden dürfen.
* Kann: Siehe Use Cases.

# Technologieauswahl

* Datenbank - H2
* Server - Spring Boot
* Client - React/TS
* Client Handler - Haskell

## Begründung H2

Eine H2-Datenbank lässt sich dank der Integration in Spring Boot mit minimalem Aufwand einsetzen. Da das zu persistierende Datenmodell klein ist (zwei Entitäten), ist H2 vorerst ausreichend. Bei Bedarf kann die Datenbank einfach durch eine andere ersetzt werden.

## Begründung Spring Boot

Spring Boot ist für Java-Webanwendungen eine beliebte Wahl, da es plattformunabhängig ist und das Bereitstellen unter verschiedenen Betriebssystemen erleichtert.

## Begründung React/TS

React mit TypeScript ermöglicht die Entwicklung moderner Webanwendungen mit Material UI für ein konsistentes Benutzererlebnis.

## Begründung Haskell

Haskell bietet sich für den Client Handler an, da es plattformunabhängig ist und ohne zusätzliche Interpreter nativ ausführbar ist.

# Architektur Modelle

## Spring-Boot-Backend

Das Backend ist nach der Onion-Architektur strukturiert, um eine klare Trennung von Geschäftslogik und Infrastruktur zu gewährleisten.

![Onion Architecture](https://imgopt.infoq.com/fit-in/3000x4000/filters:quality(85)/filters:no_upscale()/news/2014/10/ddd-onion-architecture/en/resources/onion-architecture.png)

### Infrastruktur

Die äußerste Schicht enthält Implementierungen für die Infrastruktur, z.B. zum Zippen von Dateien oder zum Verwalten des Dateisystems.

### API

Hier befinden sich die Controller für die REST-Kommunikation mit Frontend und Client Handler sowie Mapper für die DTOs.

### Domain/Core

Die Kernschicht enthält die Geschäftslogik, Modelle wie Wads und WadPacks sowie Schnittstellen für die Anwendungslogik.

### Abstraktion

Generische Klassen, z.B. für fehlerresistente Operationen, werden hier implementiert.

![Paket](https://hackmd.io/_uploads/Sk4wqsO8A.png)

## Client Handler

Die Grafik zeigt die grundlegenden Abläufe im Haskell-basierten Client Handler.

Eine **Aktion** definiert die herunterzuladenden Wads und das Startskript für ein WadPack. Die URL des Servers wird angegeben.

Die noch nicht lokal verfügbaren Wads werden ermittelt und zur Server-URL gesendet.

Der Server antwortet mit einer .zip Datei, die alle angeforderten Wads und ein .cmd Startskript enthält.

![Haskell Handler](https://hackmd.io/_uploads/B11CZioER.png)
