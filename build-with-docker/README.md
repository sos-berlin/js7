# Build the JS7 Engine

All you need is Git and Docker. The directory `build-with-docker` contains the code to build JS7 within a Docker container. You can even download the source code without using Git. Then only Docker is needed.

## Initially, clone the Git repository
```
> git clone https://github.com/sos-berlin/js7.git
```
This command clones the snapshot version. If you want to build a special version, use a tag (for example, `v2.2.0` for Version 2.2.0).

Do not use a snapshot version for production.


## Build
```
> cd js7
> git pull  # Update the code
> build-with-docker/run sbt clean-build

Or without running the tests:
> build-with-docker/run sbt clean-pack

# After several minutes you get the installation file:
> ls -l js7-install/target/universal/js7-install-*.tgz
```


## Result
* A file like `js7-install/target/universal/js7-install-*.tgz`, containing a JS7 engine installation (the jar files).
* A Docker container `js7-builder`.
* A Docker volume `js7-builder`, containing the downloaded third-party libraries.

Docker container and volume are left to speed up future builds. You can delete them.
