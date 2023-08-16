FROM rocker/tidyverse:devel
RUN install2.r LaF
COPY --chown=rstudio:rstudio ./rstudio/rstudio-prefs.json /home/rstudio/.config/rstudio