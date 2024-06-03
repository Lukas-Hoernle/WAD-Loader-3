# Protocol for communication with thist programm

* {id} denotes the id of the wadpack
* {ids,...} is a list of all wad ids used by the wadpack
* {url} is the url of the server to download wadfiles and scripts from

## wadloader://wadpack{id}-{ids,...}${url}

Download a wadpack (downloads all wads not already downloaded and a start script)


## wadloader://startwadpack{id}-{ids,...}${url}

Downloads a wadpack and starts it by using the start script (downloads all wads not already downloaded and a start script)
