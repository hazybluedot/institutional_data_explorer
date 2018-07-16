#!/usr/bin/sh

sudo docker build -t iiiexplorer/shinyproxy-template .

echo "Run systemctl restart shinyproxy for update to take effect"
