# Use the rocker/verse as the base image
FROM rocker/verse:4.3.0

# To add shiny server to a RStudio server base image (e.g. rocker/verse), see https://www.bioconductor.org/packages/devel/bioc/vignettes/sevenbridges/inst/doc/rstudio.html#3_Launch_RStudio_Server_from_Docker_container and https://github.com/sbg/sevenbridges-r/blob/master/inst/docker/sevenbridges/Dockerfile
# Install necessary dependencies for Shiny Server
RUN apt-get update && apt-get install -y \
    gdebi-core \
    && wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb \
    && gdebi -n shiny-server-1.5.16.958-amd64.deb

# Expose port 3838 for Shiny Server
EXPOSE 3838

# Config for shiny server and supervisor
COPY inst/docker/shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY inst/docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Start both Shiny Server and RStudio Server using supervisor
RUN apt-get update && apt-get install -y supervisor

# Install/set up shiny apps for shiny server e.g.
COPY . /codemapper
RUN pwd && ls && Rscript -e 'devtools::install_local(path = "/codemapper", dependencies = TRUE)'
RUN cd /srv/shiny-server && mkdir codeminer && cd codeminer && Rscript -e 'codemapper::all_lkps_maps_to_db()'
COPY inst/docker/app.R /srv/shiny-server/codeminer/app.R

# Set the entry point
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
