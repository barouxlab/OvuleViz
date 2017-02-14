##############################################################
# Load (or also install) required packages
##############################################################

if(!'pacman' %in% rownames(installed.packages())) install.packages('pacman')
pacman::p_load(shiny, shinythemes, shinyBS, ggplot2, plotly, plyr, RColorBrewer)


options(shiny.maxRequestSize = 50*1024^2) # Max upload size is 50Mbp
options(shiny.launch.browser = TRUE)

##############################################################
# Import custom color map
##############################################################

temp <- read.csv('customColorMap.csv', header = FALSE)
customcolmap <- as.character(temp[,2])
names(customcolmap) <- temp[,1]

#############################################################
# set initial cell type values
##############################################################

cellviews <- reactiveValues()

cellviews$usevp <- 'no'
cellviews$Label <- 'L1 apical'
cellviews$vp1_name <- 'vp1'
cellviews$vp2_name <- 'vp2'
cellviews$vp3_name <- 'vp3'
cellviews$viewpoint1 <-  c('L1 apical', 'L1 basal', 'L1 basal sup', 'L1 dome')
cellviews$viewpoint2 <- c('L2 apical', 'L2 basal', 'L2 basal sup')
cellviews$viewpoint3 <- NULL

# set color scheme value
cellviews$brewery <- 'Dark2'

#############################################################
# functions
##############################################################

# brewer stable color map
col.map <- function(data = data, colorize, brew){
  suppressWarnings({
    color <- brewer.pal(length(levels(data[, colorize])), brew)
  })
  names(color) <- levels(data[, colorize])
  return(color)
}

countcells <- function(data = data, vars1){
  
  # Calculate number of different cells in each stack:
  N.cells.stack <- ddply(data[data$Type == "Cell Volume",],
                         vars1,
                         summarise,
                         N = length(Value))
}

# Create viewpoints
viewpoint <- function(data = data,
                      name_vp1, vp1 = NULL,
                      name_vp2, vp2 = NULL,
                      name_vp3, vp3 = NULL){
  
  data$Viewpoints <- NA
  data[data$Labels %in% vp1, 'Viewpoints'] <- name_vp1
  data[data$Labels %in% vp2, 'Viewpoints'] <- name_vp2
  data[data$Labels %in% vp3, 'Viewpoints'] <- name_vp3
  
  data <- data[complete.cases(data$Viewpoints),]
  data$Viewpoints <- factor(data$Viewpoints)
  
  # change levels of Labels (to allow custom plotting order)
  data$Labels <- factor(data$Labels, levels = c(vp1, vp2, vp3))
  
  return(data)
}

#### Render modal output for plot download

# Download pdf workflow:

# A set of outputs sits in each tab with a plot.
# That specific plot (pl())can then be downloaded using a downloadHandler
# (handle function in Global.R) with custom values for height and width.

# The download options window comes as a BsModal/action combination
# (tagModal in Global.R, comprising 'getpdf_xxx' and 'getpdf_xxx_Button'), that also provides
# the inputs for custom heigth and width ('getpdf_xxx_height' and 'getpdf_xxx_width').

# Each tab calls a specfic handle() and a tagModal().
# This is because the same reactive cannot be used twice

tagModal <- function(x){
  
  tagList(
    bsModal(x, "Export current plot as a pdf",
            trigger = paste0(x, "_Button"), size = "small",
            numericInput(paste0(x, '_height'), label = 'Plot height (cm)',
                         value = 15,  min = 1, max = 50, step = 0.1),
            numericInput(paste0(x, '_width'), label = 'Plot width (cm)',
                         value = 25,  min = 1, max = 50, step = 0.1),
            downloadButton(x, "Download")),
    
    actionButton(paste0(x, "_Button"), "Export PDF",
                 class = 'downloadBut_style')
  )
}

handle <- function(plot, width, height){
  
  downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {ggsave(file, plot = plot,
                                     device = 'pdf', units = "cm",
                                     width = width,
                                     height = height
    )})
}

# calculate standard error
sem <- function(x) sd(x)/sqrt(length(x))

##############################################################
# ANOVA
##############################################################

# Calculate p-value for full model

calcAnova <- function(x, variable){
  # check http://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html
  
  f <- paste('Value', '~', variable)
  anova <- do.call("aov", list(as.formula(f), data = x))
  fst <- summary.lm(anova)$fstatistic
  p <- pf(fst[1], fst[2], fst[3], lower.tail = F)
  attributes(p) <- NULL
  return(paste('p =', signif(p, 3)))
}

# Potential alternative: non-parametric ANOVA
calcKruskal <- function(x, variable){
  f <- paste('Value', '~', variable)
  k <- do.call("kruskal.test", list(as.formula(f), data = x))
  p <- k$p.value
  return(paste('p =', signif(p, 3)))
}

# Distribute p-values through facets

facet_ANOVA <- function(x, facet, fill) {
  df <- ddply(x,
              facet,
              calcAnova, variable = fill)
  
  names(df) <- c(names(df)[1], 'pval')
  return(df)
}

# add ANOVA p-value to plots
addANOVA <- function(x = p, origin = data, face, col){
  
  xrange <- ggplot_build(x)$layout$panel_ranges[[1]]$x.range
  yrange <- ggplot_build(x)$layout$panel_ranges[[1]]$y.range
  xpos <- xrange[1] + (xrange[2] - xrange[1])*0.3
  ypos <- yrange[1] + (yrange[2] - yrange[1])*0.9
  
  try({
    x <- x + geom_text(data = facet_ANOVA(origin,
                                        face, 
                                        col),
                     x = xpos, y = ypos,
                     size = rel(5), color = 'grey70',
                     aes(label = pval),
                     inherit.aes = FALSE)
  })
  return(x)
}

##############################################################
# Options colorize and group menus
##############################################################

GLSN <- c('Genotype', 'Labels', 'Stage', 'neighbourSMC')
GLSNS <- c('Genotype', 'Labels', 'Stage', 'neighbourSMC', 'Stack')
GLVSN <- c('Genotype', 'Labels', 'Viewpoints', 'Stage', 'neighbourSMC')
GLVSNS <- c('Genotype', 'Labels', 'Viewpoints', 'Stage', 'neighbourSMC', 'Stack')

##############################################################
# style
##############################################################

# set plotting options
theme_set(theme_minimal(18))

ggoptions <- function(p, title, sclLog, tgr){
  p <- p + ggtitle(title)
  if(sclLog){p <- p + scale_y_log10()}
  if(tgr){p <- p + theme_grey(18)}
  return(p)
}

