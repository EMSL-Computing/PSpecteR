## David Degnan, Pacific Northwest National Labs
## Last Updated: 2021_04_01

## PSpecteR FULL IMAGE Version: 1.0.1
## Tag: pspecter:1.0.1

## You must first build Dockerfile_v.1.0.1_BaseImage in Old Docker Files
FROM docker.artifactory.pnnl.gov/pspecter/pspecterbase:1.0.1

# Copy configuration files into the Docker image
WORKDIR /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY Glossary /srv/shiny-server/Glossary
COPY Licenses /srv/shiny-server/Licenses
COPY Server /srv/shiny-server/Server
COPY www /srv/shiny-server/www
COPY data /data
COPY SetEnvironment.csv /SetEnvironment.csv
COPY TestFiles /TestFiles

# Make the ShinyApp available at port 3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]