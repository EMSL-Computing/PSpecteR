## David Degnan, Pacific Northwest National Labs
## Last Updated: 2023_05_01

## PSpecteR FULL IMAGE Version: 2.0.0
## Command: docker build -t docker.artifactory.gov/pspecter/pspecter:2.0.0 .
## Run: docker run -p 2753:2753  docker.artifactory.gov/pspecter/pspecter:2.0.0

## You must first build Dockerfile_v.1.0.1_BaseImage in Old Docker Files
FROM docker.artifactory.pnnl.gov/pspecter/pspecterbase:2.0.0

# Copy configuration files into the Docker image - PSpecteR Light Version only
WORKDIR /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY Licenses /srv/shiny-server/Licenses
COPY Server /srv/shiny-server/Server
#COPY data /data
COPY SetEnvironment.csv /SetEnvironment.csv
COPY TestFiles /srv/shiny-server/TestFiles
COPY www /srv/shiny-server/www



# Make the ShinyApp available at port 2753
EXPOSE 2753

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 2753, launch.browser = FALSE)"]