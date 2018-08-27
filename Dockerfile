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

RUN R -e "install.packages(c('shiny', 'shinytest', 'rmarkdown'), repos='https://cloud.r-project.org/'); \
shinytest::installDependencies()"

# install dependencies of the III app
RUN R -e "install.packages(c('tidyverse', 'DT', 'rms', 'shinythemes', 'devtools', 'sjPlot'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"

# install more dependencies of the III app
RUN R -e "devtools::install_github('hazybluedot/ShinySky'); devtools::install_github('hazybluedot/racadia')"

# copy the app to the image
RUN mkdir /root/deepr /root/data /root/vtir /root/IIIExplorer
COPY deepr /root/deepr
COPY vtir /root/vtir
# COPY data /root/data

RUN R -e "devtools::install('/root/deepr'); devtools::install('/root/vtir')"

COPY IIIExplorer /root/IIIExplorer

RUN R -e "source('/root/IIIExplorer/init.R'); init_data()"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/IIIExplorer')"]
