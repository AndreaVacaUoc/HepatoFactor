# Hepatic Fibrosis Degree Prediction

Hepatic Fibrosis Degree Prediction Web App to make the predictions, we use a classification model* created with sequential neural network algorithm and trained with the [HCV-Egy-Data (Hepatitis C Virus (HCV) for Egyptian patients)](https://archive-beta.ics.uci.edu/dataset/503/hepatitis+c+virus+hcv+for+egyptian+patients) data set was obtained from the [UCI machine learning repository](https://archive-beta.ics.uci.edu/).

## How to try it

You can access and try the web application from this [link](https://andreavacauoc.shinyapps.io/HepatoFactor/)!

## Programming languages

To create the machine learning models, the statistical software *R* version 4.2.3 has been used together with the integrated development environment (IDE) *RStudio* version 2023.03.0 + 386, allowing to work comfortably with the R programming language. 

The main R packages used to create machine learning models are *dplyr* for dataset management, *ggplot2* for graph creation, *keras* and *tensorflow* for creating machine learning model.

On the other hand, the main packages used for the creation of the web application are *shiny* and *shinydashboard* for the creation of the web application, *ggplot2* and *plotly* to generate plots and *shinyjs*, *shinyFeedback*, *shinyalert* and *waiter* to add different functionalities to the application.

The "Hepatofactor" folder contains all the elements of the application, open app.R and run the application.

The file models.R contains the analysis of the machine learning algorithms evaluated.

* You could obtain more information about how the model was created and the data used to train it in the paper of [Vaca Tello, A (2023) called “Desarrollo de aplicación web para diagnóstico no invasivo de fibrosis hepática utilizando técnicas de aprendizaje automático”.](https://drive.google.com/open?id=10lthbcxZITKT2jEoa90MDP02d0ymlzic&usp=drive_fs)