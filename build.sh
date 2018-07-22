#!/usr/bin/sh

R_PACKRAT_CACHE_DIR=../packrat_cache
R -e "packrat::init()"

sudo docker build -t iiiexplorer/shinyproxy-template .

#sudo docker run -t iiiexplorer/shinyproxy-template test 

echo "Run systemctl restart shinyproxy for update to take effect"
