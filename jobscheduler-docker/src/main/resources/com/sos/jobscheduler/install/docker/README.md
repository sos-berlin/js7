## Example Docker installation

Sources for `target/universal/jobscheduler-docker-(version).tgz`,
containing file for a example docker-compose configuration
including one Master and two Agent containers.

### Prequisites

* [Docker](https://docs.docker.com/)
* [docker-compose](https://docs.docker.com/compose/install/)
* bash
* Internet connection (to Docker registry)
* The archives jobscheduler-docker-_version_.tgz and jobscheduler-_version_.tgz.
* If you build JobScheduler yourself, you can find these generated files under
  * jobscheduler-docker/target/universal/jobscheduler-docker-_version_.tgz
  * jobscheduler-install/target/universal/jobscheduler-install-_version_.tgz
  
`build/bin/prepare` is a little script to help you to prepare docker installation with a example configuration.   
    
### Usage

    # Create a new directory (for example "jobscheduler-docker")
    mkdir jobscheduler-docker
    cd jobscheduler-docker

    # Unpack bootstrap script `build/bin/prepare`
    tar xzf .../jobscheduler-docker-(version).tgz build/bin/prepare
    
    # Prepare and build Docker image and containers. Restart here if JobScheduler has been updated.
    build/bin/prepare \
      -docker.tgz=.../jobscheduler-docker-(version).tgz \
      -install.tgz=.../jobscheduler-install-(version).tgz

    # Or, if you build the Git repository yourself, you can name the development directory with -dev=. 
    # -dev= denotes the directory of the local Git repository "jobscheduler" (built with sbt)..
    # Defaults to environment variable JOBSCHEDULER_DEV.
    build/bin/prepare -dev=.../jobscheduler
    
    # Note: "prepare" replaces the content of the directory build/. Do not change anything in build/.

    # Run Docker containers
    docker-compose up
   
The directory `volume` contains your master's and agents configuration and data files.
The script "prepare" does not overwrite any configuration file.

    # To clean the volumes data directories (for a clean restart), keeping only the configuration
    build/bin/clean-volumes-data

    # To reset the configurations and data
    rm -rf volumes/
    build/bin/prepare ... (like above)

### Update

After a JobScheduler update, rebuild Docker containers with with `build/bin/prepare`, keeping your volumes.
