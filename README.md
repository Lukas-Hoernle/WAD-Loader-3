# WAD-Loader-3

Monorepo for online Doom-Wad Management System.

## Todos

Backend:
* remove created zipfiles periodically (download is valid for x-minutes, maybe 5 or 15 minutes), configurable 

Frontend:
*  create "Upload-Wad" Page
*  create "Edit Wad" Page 
*  create "Create WadPack" Page
*  create "Edit WadPack" Page
*  create "Search/Download" Page (one Page for Wads and WadPacks at once should be sufficient)
  * this page needs a search function for Wads/WadPacks
    * based on the name
    * based on the description    

Client-Handler:
* maybe cleanup of .zip files
  
Optional:
* Possibility to add Tags to Wads and WadPacks
  * Wads/WadPacks should then be searchable by these packs
* Comments for Wads/WadPacks 

## Setup

Necessary setup to run the program.
Setting localhost:3000 is only necessary if you want to run the frontend on it's own.

1. Create Authorization Project at auth0
    * create free account at https://auth0.com
    * Create a new "Regular Web Application"
    * add "allowed callback urls": http://localhost:8080/login/oauth2/code/okta
    * add "allowed logout urls": http://localhost:3000,http://localhost:8080
    * add "allowed web origins": http://localhost:3000,http://localhost:8080
    * check "allow corss-origin authentication" and add origins: "http://localhost:3000, http://localhost:8080"
2. create file src/main/resources/application-local.properties
3. add entries for (okta.XXX values are from the auth0 project created previously):
    * spring.datasource.username
    * spring.datasource.password
    * okta.oauth2.issuer
    * okta.oauth2.client-secret
    * okta.oauth2.client-id

## Build Application

These steps help you to create an executable jar file.

1. cd ./wadloader3
2. npm run build
3. mvn package

## Start application

Start the created jar file. No parameters needed.

* java -jar "path/to/jar/from/previous/step.jar"

## Use Application as a User

1. download the setup script
2. run the setup script as a user
3. set %GZDOOM_PATH% to your gzdoom.exe
4. set %IWAD_PATH% to the iwad you want to use to start your WadPacks