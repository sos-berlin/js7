## Example Docker installation


Sources for `target/universal/jobscheduler-docker-(version).tgz`,
containing file for a docker-compose configuration
including one Master and two Agent containers.

### Prequisites

* [Docker](https://docs.docker.com/)
* [docker-compose](https://docs.docker.com/compose/install/)
* Internet connection (to Docker registry and Alpine Linux package repository)
    
### Usage

    # Create a new directory (for example "jobscheduler-docker")
    mkdir jobscheduler-docker
    cd jobscheduler-docker
         
    # Unpack bootstrap script `build/bin/prepare`
    tar xzf .../jobscheduler/jobscheduler-docker/target/universal/jobscheduler-docker-(version).tgz \
      build/bin/prepare
    
    # In case there is a previously pulled base image we want to update 
    docker pull openjdk:8-jre-alpine
    
    # Prepare and build Docker image and containers. 
    # Restart here if JobScheduler has been updated.
    # -dev= denotes the root directory of the sbt-built Git repository "jobscheduler".
    # (You may preset the environment variable JOBSCHEDULER_DEV.)
    build/bin/prepare -dev=.../jobscheduler
    
    # Or if you don't want to build the Git repository and prefer to name the two tars directly
    build/bin/prepare \
      -jobscheduler-docker.tgz=.../jobscheduler-docker-(version).tgz \
      -jobscheduler.tgz=.../jobscheduler-docker-(version).tgz

    # Note: "prepare" replaces the content of the directory build/.

    # Run Docker containers
    docker-compose up
   
The directory `volume` contains your master's and agents configuration and data files.
The script "prepare" does not overwrite any configuration file.

    # To clean the volumes data directories (for a clean restart), keeping the configuration
    build/bin/clean-volumes-data

    # To reset the configurations and data
    rm -rf build/
    build/bin/prepare ... (like above)

### Update

After a JobScheduler update, rebuild Docker containers with with `prepare`.

