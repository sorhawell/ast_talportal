# 2023 written by JONAS JONASSON and sorhawell@gmail.com
FROM rocker/shiny-verse
COPY deploy/install_r_packages.R /srv/R_files/
RUN Rscript /srv/R_files/install_r_packages.R
