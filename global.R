##############################################################
# Load (or also install) required packages
##############################################################

if(!'pacman' %in% rownames(installed.packages())) install.packages('pacman')
pacman::p_load(shiny, shinythemes, shinyBS, ggplot2, plotly, plyr, RColorBrewer, tcltk)


##############################################################
# Import segmented data
##############################################################

# data <- read.csv2(
#   tk_choose.files(caption = "Import file with segmented data",
#                   multi = FALSE,
#                   filters = matrix(c("csv", ".csv"), 1, 2, byrow = TRUE)),
#   row.names = NULL)

data <- read.csv2('~/segmented.csv', row.names = NULL)

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
#cellviews$vp1_name <- 'vp1'
#cellviews$viewpoint1 <- c('L1 apical', 'L1 basal', 'L1 basal sup', 'L1 dome')
#cellviews$vp2_name <- 'vp2'
#cellviews$viewpoint2 <- c('L2 apical', 'L2 basal', 'L2 basal sup')
#cellviews$vp3_name <- 'vp3'
#cellviews$viewpoint3 <- c('SMC')

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


groupmeans <- function(data, vars2, Ncells = N.cells.stack){
  
  # calculate means and std deviations
  temp <- ddply(data, c(vars2, "Type"),
                summarise,
                mean = mean(Value),
                sd   = sd(Value),
                nCells = length(Value),
                nStacks = length(unique(Stack)))
  
  # Average number of cells per stacks:
  N.cells.mean <- ddply(Ncells,
                        vars2,
                        summarise,
                        Type = "Cell Number",
                        mean = mean(N),
                        sd   = sd(N),
                        nCells = NA,
                        nStacks = length(N))
  
  # Add info on cell numbers:
  group.summary <- rbind(temp, N.cells.mean)
  
  # round
  group.summary$mean <- round(group.summary$mean, 3)
  group.summary$sd <- round(group.summary$sd, 3)
  return(group.summary)
}


# Create viewpoints
viewpoint <- function(data = data,
                      name_vp1, vp1 = NULL,
                      name_vp2, vp2 = NULL,
                      name_vp3, vp3 = NULL
){
  data$Viewpoints <- NA
  data[data$Labels %in% vp1, 'Viewpoints'] <- name_vp1
  data[data$Labels %in% vp2, 'Viewpoints'] <- name_vp2
  data[data$Labels %in% vp3, 'Viewpoints'] <- name_vp3
  data <- data[complete.cases(data$Viewpoints),]
  
  # change levels of Labels (to allow custom plotting order)
  data$Labels <- factor(data$Labels, levels = c(vp1, vp2, vp3))
  
  return(data)
}

#### Render modal output for plot download

# Download pdf workflow is slightly complex: a set of outputs sits in each tab with a plot.

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

##############################################################
# calculate number of cells per ovule
##############################################################

N.cells.stack <- countcells(data, c("Stack", "Genotype", "Stage", "Labels"))

