shinyUI(
  navbarPage(
    "v1.1", theme = shinytheme("cosmo"),
    
    tabPanel(
      "Plots",
      fluidPage(
        fluidRow(
          verbatimTextOutput('debug'),
          column(2,
                 
                 #######################
                 h3("Data selection"),
                 
                 checkboxGroupInput('Genotype',
                                    label = 'Genotypes',
                                    choices = levels(data$Genotype),
                                    selected = levels(data$Genotype)),
                 
                 tags$hr(),
                 
                 
                 checkboxGroupInput('Stage',
                                    label = 'Stages',
                                    choices = levels(data$Stage),
                                    selected = levels(data$Stage)),
                 
                 actionButton('Stage_goButton', 'select all',
                              style = actbut_style, width = '60%'),
                 
                 tags$hr(),
                 
                 strong('Tag filtering'),
                 
                 checkboxInput('SMCneighb',
                               label = 'only SMC neighbour cells',
                               value = FALSE),
                 
                 #####################
                 
                 tags$hr(),
                 
                 h3('Cell types'),
                 
                 h5('Use viewpoints?'),
                 
                 radioButtons('usevp',
                              label = NULL,
                              choices = c('yes', 'no'),
                              selected = 'no'),
                 
                 uiOutput('views')
          ),
          
          column(2,
                 
                 #####################
                 h3("Plot elements"),
                 
                 selectInput("Yaxis",
                             label = "Variable:",
                             choices = c(levels(data$Type), 'Cell Number')),
                
                 radioButtons('group',
                              label = 'Split plots on:',
                              choices = GLSN,
                              selected = 'Labels'),
                 
                 radioButtons('colorize',
                              label = 'Color on:',
                              choices = GLSN,
                              selected = 'Genotype'),
                 
                 conditionalPanel("input.tabs1 == 'histogram'", style = panel_style,
                                  h3('Histogram options'),
                                  radioButtons('density', label = 'Y-axis:',
                                               choices = c('counts', 'density'),
                                               selected = 'density'),
                                  sliderInput('bin', label = 'Number of bins', ticks = FALSE,
                                              min = 2, max = 100, value = 40,
                                              step = 2, width = '60%')),
                 
                 tags$hr(),
                 
                 h3("Graphic options"),
                 
                 conditionalPanel("input.tabs1 == 'scatter'",
                                  style = panel_style,
                                  checkboxInput('interact', label = 'Interactive plot (slower)',
                                                value = FALSE)),
                 
                 checkboxInput('logY', label = 'log y-axis',
                               value = FALSE),
                 
                 checkboxInput('gtheme', label = 'grey background',
                               value = FALSE),
                 
                 sliderInput('plheight', label = 'Plot height', ticks = FALSE,
                             min = 200, max = 2000, value = 600, step = 200, width = '60%'),
                 
                 sliderInput('ncols', label = 'Plots per row', ticks = FALSE,
                             min = 1, max = 6, value = 2, step = 1, width = '60%'),
                 
                 helpText("Color scheme (http://colorbrewer2.org)"),
                 
                 uiOutput('brewing'),
                 
                 checkboxInput('customcol', label = 'Use custom color mode for cell types',
                               value = TRUE)

          ),
          
          column(8,
                 tabsetPanel(id = 'tabs1',
                             tabPanel('boxplot',
                                      uiOutput('UIgetpdf_bx'),
                                      plotOutput('plotBoxplot')),
                             tabPanel('scatter',
                                      conditionalPanel("input.interact == false",
                                                       uiOutput('UIgetpdf_jit'),
                                                       plotOutput('plotJitter')),
                                      conditionalPanel("input.interact == true",
                                                       plotlyOutput('jitter_i', height = 'auto'),
                                                       #textOutput("selectTable")
                                                       dataTableOutput("selectTable")
                                                       )),
                             tabPanel('histogram',
                                      uiOutput('UIgetpdf_h'),
                                      plotOutput('plotHist')),
                             tabPanel('means',
                                      helpText('Error bars represent standard deviation.'),
                                      uiOutput('UIgetpdf_mn'),
                                      actionButton('MeanTabButton', label = 'Table with values',
                                                   style = actbut_style),
                                      bsModal('tabMeansMod', 'Mean ± SD', 'MeanTabButton',
                                              dataTableOutput('tableMeans')),
                                      plotOutput('plotMeans')),
                             tabPanel('table',
                                      dataTableOutput('gg_data_table'))
                             )
          )
        ))),
    tabPanel("Tables",
             fluidPage(
               
               tabsetPanel(id = 'tabs2',
                           tabPanel('Original data', dataTableOutput("alldata")),
                           tabPanel('Cells per stack', dataTableOutput('cell_numb_stack'),
                                    helpText('Download full table as a .CSV file'),
                                    downloadButton('downloadCellsStack', 'Download')),
                           tabPanel('Means + StDev',
                                    dataTableOutput('group_summary'),
                                    helpText('Download full table as a .CSV file'),
                                    downloadButton('downloadMeansSD', 'Download'))

                           
               )))
  ))
