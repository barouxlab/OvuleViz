# OvuleViz
Graphical interface to analyse segmented cell data

v1.1 (30/01/2017)

#### To start the analysis run (from an R console):
```R
library(shiny)
runGitHub( "OvuleViz", "piresn")
```

#### To create a segmented data file from Imaris output run (from an R console):
```R
library(RCurl)
u <- 'https://raw.githubusercontent.com/piresn/OvuleViz/master/getData.R'
script <- getURL(u, ssl.verifypeer = FALSE)
eval(parse(text = script))
```
