FROM openanalytics/r-base

MAINTAINER Darren Maczka "dmaczka@vt.edu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libxml2-dev 

# basic shiny functionality

RUN R -e "install.packages(c('packrat', 'shiny', 'shinytest', 'rmarkdown', 'devtools'), repos='https://cloud.r-project.org/'); \
shinytest::installDependencies()"

# install dependencies of the III app

# copy the app to the image
RUN mkdir /root/local/deepr \
  /root/packrat_cache \
  /root/data /root/IIIExplorer \
  /root/IIIExplorer/packrat

COPY IIIExplorer/packrat/packrat.lock /root/IIIExplorer/packrat
COPY IIIExplorer/packrat/packrat.opts /root/IIIExplorer/packrat

COPY packrat_cach /root/packrat_cache

COPY deepr /root/local/deepr
# COPY data /root/data

RUN R -e "devtools::install('/root/deepr')"

COPY IIIExplorer /root/IIIExplorer


RUN R -e "packrat::packify(); packrat::restore()"

RUN R -e "source('/root/IIIExplorer/init.R'); init_data()"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/IIIExplorer')"]
