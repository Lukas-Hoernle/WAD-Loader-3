# WAD-Loader-3

Monorepo for online Doom-Wad Management System.

## Todos

Backend:
* implement Api on the Server
   * delegate logic in ApiController to WadPackService and WadService
* implement logic
* create endpoint to download WadPack
* create Command to start and unzip wadpack

Frontend:
*  create "Upload-Wad" Page
*  create "Edit Wad" Page 
*  create "Create WadPack" Page
*  create "Edit WadPack" Page
*  create "Download" Page (one Page for Wads and WadPacks should be sufficient)

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
