# WAD-Loader-3

Monorepo for online Doom-Wad Management System.

## Setup

Necessary setup to run the program.
Setting localhost:3000 is only necessary if you want to serve the frontend from vite.

1. Create Authorization Project at auth0
    * create free account at https://auth0.com
    * Create a new "Regular Wnpmeb Application"
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

you need: 
* java: 21
* haskell: GHC2021
* node: 20 (lower should be possible as well)
* npm (js/ts build tool)
* yalc (manager for local npm packages ,can be installed via npm)
* maven (java build tool)
* cabal (haskell build tool, recommended installation via ghcup)

These steps help you to create an executable jar-file.
All steps assume the repository root as initial dir.

### Build the api

1. cd ./api-generator
2. npm install
3. npm run generate
4. npm run update

### Build haskell client (can be omitted if no changes to the haskell code were made)

1. cd ./local-client
2. cabal build
3. cp ./dist-newstyle/build/x86_64-windows/ghc-9.4.8/local-client-0.1.0.0/x/local-client/build/local-client/local-client.exe ../wadloader3/src/main/resources/local-client.exe

### Build the back- and frontend

1. cd ./wadloader3
2. yalc add wadloader3-api
3. npm install
4. npm run build
5. mvn package

## Start server

Start the application without additional parameters.

## Setup Application as a User

1. Login
2. download the setup script (by clicking the download button or calling /download/setup)
3. run the setup script with admin privileges (it can also elevate itself)

optional (if you don't set these you are asked about their values every time you start a wadpack):
4. set %GZDOOM_PATH% as the path to your gzdoom.exe
5. set %IWAD_PATH% to the path of iwad you want to use to start your WadPacks