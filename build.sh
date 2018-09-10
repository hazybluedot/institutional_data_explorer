#!/usr/bin/sh

sudo docker build -t iiiexplorer/shinyproxy-grade_matrix .

echo "Run systemctl restart shinyproxy for update to take effect"
