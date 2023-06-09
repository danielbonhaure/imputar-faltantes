
##################################################################
##                           README                             ##
##################################################################
## Este Dockerfile permite crear un contendor con R y todos los ##
## paquetes necesarios para imputar los faltantes la serie tem- ##
## poral de una estación del CRC-SAS, y ademas, calcular la ra- ##
## diación diaria para la serie completa.                       ##
##################################################################



#################################
## Stage 1: Install R packages ##
#################################


# Create image
FROM r-base

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq --no-install-recommends install \
        # to install httr (R)
        # to install s2, a gstat dependency (R)
        libssl-dev \
        # to install units, a gstat dependency (R)
        libudunits2-dev \
        # to install sf, a gstat dependency (R)
        gdal-bin libgdal-dev && \
    rm -rf /var/lib/apt/lists/*

# Set CRAN mirror
ARG CRAN_MIRROR="getOption('repos')"

# Install R packages
RUN R -e "options(warn=2); install.packages('doMC', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('dplyr', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('glue', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('gstat', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('here', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('httr', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('jsonlite', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('missForest', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('optparse', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('purrr', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('randomForest', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('robustbase', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('sirad', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('sp', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('tibble', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('tidyr', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('xts', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('zoo', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('lazyeval', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('rgeos', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('geosphere', repos=${CRAN_MIRROR}, verbose=T)"
RUN R -e "options(warn=2); install.packages('rgdal', repos=${CRAN_MIRROR}, verbose=T)"

COPY . /opt



#####################################################
## Usage: Commands to Build and Run this Container ##
#####################################################


# Build container
#
# docker build --file Dockerfile --tag imputador:latest .

# Run container
#
# docker run --name imputador --volume /tmp:/tmp --rm imputador:latest \
# Rscript /opt/Main.R -s 86246 -c PY -m 4 -o /tmp/minga.csv
