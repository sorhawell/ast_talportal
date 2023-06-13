#!/bin/bash
pwd
docker run -v .:/srv/shiny-server/ --rm -p 3838:3838 rocker/shiny-verse