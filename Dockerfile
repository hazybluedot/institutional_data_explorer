FROM r-base:latest

MAINTAINER Darren Maczka "dmaczka@vt.edu"

## From rocker/shiny
## https://hub.docker.com/r/rocker/shiny/

## Install dependencies and Download and install shiny server

## See https://www.rstudio.com/products/shiny/download-server/ for the
## instructions followed here.

RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev && \
    wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
R -e "install.packages(c('shiny', 'shinytest', 'packrat', 'rmarkdown'), repos='https://cran.rstudio.com/')" && \
    rm -rf /var/lib/apt/lists/*

#  copy the app to the image
RUN mkdir -p /root/local/deepr \
  /root/packrat_cache \
  /root/data /root/IIIExplorer \
  /root/IIIExplorer/packrat

COPY IIIExplorer/packrat/packrat.lock /root/IIIExplorer/packrat
COPY IIIExplorer/packrat/packrat.opts /root/IIIExplorer/packrat

COPY packrat_cache /root/packrat_cache
ENV R_PACKRAT_CACHE_DIR /root/packrat_cache

COPY local/deepr /root/local/deepr
# COPY data /root/data

# RUN R -e "devtools::install('/root/deepr')"

COPY IIIExplorer /root/IIIExplorer

RUN R -e "setwd('/root/IIIExplorer'); packrat::restore()"

#RUN R -e "source('/root/IIIExplorer/init.R'); init_data()"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/IIIExplorer')"]
