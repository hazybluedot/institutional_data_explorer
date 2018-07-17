# Academic Data Explorer

This is a Shiny-based interface to academic data. Currently it is tailored for data generated by Virginia Tech without a straight-forward process for adapting for other data strutures, however as the project becomes more mature it is likely data structure information will be consolodated into a config file, allowing for re-use in different environments.

# Installation

# Server stuff

## Tell SELinux to give nginx permission to the www director

````
sudo setsebool -P httpd_can_network_connect on
sudo chcon -Rt httpd_sys_content_t /var/www
````
