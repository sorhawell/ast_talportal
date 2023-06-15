#!/bin/bash
pwd
docker build -t ast_shiny_0.1.1 . #update image
docker image prune -f # remove previous build image
docker run -v .:/srv/shiny-server/ --rm -p 3838:3838 -p 3839:3839 ast_shiny_0.1.1
