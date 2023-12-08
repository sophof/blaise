FROM rocker/tidyverse:devel
RUN install2.r LaF spelling
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  qpdf
COPY --chown=rstudio:rstudio ./rstudio/rstudio-prefs.json /home/rstudio/.config/rstudio