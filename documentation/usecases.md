# Detaillierte Problemstellung
* Multiplayer Doom Spielen mit Mods ist schwierig weil:
    * Jeder braucht alle Mods
    * Jeder muss die selben Mods in der gleichen Reihenfolge haben
    * Keine standardtisierte Lösung
    * clientseitige Software Optional => kein lokaler install notwendig

# Use Cases
* .wad Dateien Archiv (upload/download wads)
* Suche über .wad-Dateien (by Name, optional über Eigenschaften z.B. Pfade))
* Login/ Logout
* Nutzerverwaltung
* Gruppieren von mehreren .wad-Dateien in Reihenfolge in "WadPacks"
    * Bearbeiten der eigenen Packs
    * Optional Kopieren der Packs anderer
* Download von einzelnen Dateien oder ganzen WadPacks
* Erstellen von Shell-Kommando zum Starten von Wads
* Optional start von Wads oder Wadspacks über lokalen handler
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
* Muss Disclaimer haben, dass nur Mods mit allen Rechten hochgleaden werden dürfen
* Rest: siehe Use Cases

# Technologieauswahl
* H2 Datenbank? Oder postgres SQL
* Spring Boot Server
* React/ TS Client
* ??? Für optionalen client side Handler
