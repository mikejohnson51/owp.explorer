FROM rocker/shiny:4.1.2

RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libglpk-dev libgmp-dev libicu-dev libpng-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
# Install R packages.
#
RUN install2.r -e shiny
RUN install2.r -e leaflet
RUN install2.r -e dplyr
RUN install2.r -e sf
RUN install2.r -e arrow
RUN install2.r -e aws.s3

RUN rm -rf /srv/shiny-server/
  # Copy app.
  #
COPY explore/app.R /srv/shiny-server/app.R
