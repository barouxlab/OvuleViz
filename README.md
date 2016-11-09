# OvuleViz
Graphical interface to analyse segmented cell data
v1.0 09/11/2016

#### To start the analysis run (from an R console):
library(shiny)</br>
runGitHub( "OvuleViz", "piresn", launch.browser = TRUE)


#### To create a segmented data file from Imaris output run (from an R console):
library(RCurl)<br/>
u <- 'https://raw.githubusercontent.com/piresn/OvuleViz/master/getData.R'<br/>
script <- getURL(u, ssl.verifypeer = FALSE)<br/>
eval(parse(text = script))
