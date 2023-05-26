FROM rocker/r-ver:4.1.0

RUN R -e "install.packages(c('reticulate', 'shinyjs', 'h5py', 'shiny', 'shinydashboard', 'shinyFeedback', 'shinyalert', 'caret', 'recipes', 'dplyr', 'ggplot2', 'plotly', 'rlang', 'waiter', 'keras', 'tensorflow'))"

RUN apt-get update && apt-get install -y python3-pip
RUN pip3 install h5py tensorflow

COPY src /src
WORKDIR /src
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]