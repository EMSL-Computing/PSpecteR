This Docker container runs PSpecteR, a Shiny web application for tandem LC-MS proteomic data quality control and research. The container is built with a rocker shiny image for R 3.6.1 and includes PhantomJS for image exportation and mono for the R's raw file reading package (rawDiag). The app also contains interfaces to run containerized versions of the peptide database search algorithms MS-GF+ (for bottom-up proteomics) and MSPathFinderT (for top-down proteomics). 

### USAGE

To run the PSpecteR docker container and its two companion docker containers, build each container *within the container directory*: 

`docker build -t pspecter:1.0.0 .`

`docker build -t msgf:1.0 .`

`docker build -t mspathfindert:1.0 .`

Then define the shared folder between the containers and use the docker compose file in the pspecter_container repo: 

`export PSPECTER_DATA:/path/to/folder/with/data`

`docker-compose up`

You may also run the pspecter_container by itself, but note that MS-GF+ and MSPathFinderT will not run: 

`cd /path/to/pspecter_container`

`docker run -v ${PWD}:/data --rm -p 3838:3838 pspecter:1.0.0`
