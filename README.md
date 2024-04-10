# WAD-Loader-3

Monorepo for online Wad Management System.

Consists of a Spring Boot Server and a React Frontend.
The Api is defined using the open api specification and the openapi generator

## TODO
* Redirect treaffic if content is server from spring
  * use Redirect/mutate in WebSecurity Filter chain
  * seperate set of possible paths in frontend and backend/api paths
  * create seperate authentication page e.g. ditch default spring form Login
  * maybe use auth0 for authentication   
