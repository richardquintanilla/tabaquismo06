FROM rocker/shiny:4.4.0

# 1. Instalar dependencias del sistema necesarias para 'sf', 'fst' y 'chilemapas'
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libnode-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar paquetes de R
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'dplyr', 'tidyr', 'ggplot2', 'plotly', 'reactable', 'htmltools', 'fst', 'writexl', 'janitor', 'sf', 'glue', 'openxlsx', 'remotes'))"

# 3. Instalar chilemapas desde GitHub (no está en CRAN)
RUN R -e "remotes::install_github('pachamaltese/chilemapas')"

# 4. Crear carpeta y copiar archivos
RUN mkdir -p /srv/shiny-server

COPY app.R /srv/shiny-server/
COPY data /srv/shiny-server/data/
COPY www /srv/shiny-server/www/

# 5. Exponer puerto y ejecutar
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
