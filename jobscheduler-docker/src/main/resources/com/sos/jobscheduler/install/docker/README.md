## Example Docker installation

This examples includes one Master and two Agent containers, running some workflows.

### Prequisites

* [Docker](https://docs.docker.com/)
* [docker-compose](https://docs.docker.com/compose/install/)
* bash
* Internet connection (to access the Docker registry)
* The archives 
  * jobscheduler-docker-_version_.tgz
  * jobscheduler-install-_version_.tgz.
* If you build JobScheduler yourself, you can find these generated files under
  * jobscheduler-docker/target/universal/jobscheduler-docker-_version_.tgz
  * jobscheduler-install/target/universal/jobscheduler-install-_version_.tgz
  
`build/bin/prepare` is a little script to help you to prepare docker installation with a example configuration.   
    
### Usage

Create a new directory.

    mkdir jobscheduler-docker
    cd jobscheduler-docker
    
Place jobscheduler-docker-_version_.tgz and jobscheduler-install-_version_.tgz in this directory.

    # For example if you have build the JobScheduler yourself: 
    cp .../development/jobscheduler/jobscheduler-docker/target/universal/jobscheduler-docker-*_.tgz .
    cp .../development/jobscheduler/jobscheduler-install/target/universal/jobscheduler-install-*_.tgz .

Unpack bootstrap script `build/bin/prepare`

    tar xzf jobscheduler-docker-(version).tgz build/bin/prepare
    
Prepare and build Docker image and containers.

    build/bin/prepare 
    
Note: `prepare` replaces the content of the directory build/.
Do not change anything in build/.
It is used to build the Docker image.
It is not needed to run the docker images.
You may delete this directory after the script has been run, if you don't want to use any of the scripts in `build/bin`.

Run Docker containers.

    docker-compose up
   
The directory `volume` contains your master's and agents configuration and data files.
The script "prepare" does not overwrite any configuration file.

To clean the volumes data directories (for a clean restart), keeping only the configuration:

    build/bin/clean-volumes-data

To reset the configurations and data:

    rm -rf volumes/
    build/bin/prepare ... (like above)

### Update

After a JobScheduler update, rebuild Docker containers with `build/bin/prepare`.
This script does not touch the configuration files in directory `volumes`. 
