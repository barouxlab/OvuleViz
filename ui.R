shinyUI(
  navbarPage(
    "v1.1", theme = shinytheme("cosmo"),
    
    tabPanel(
      "Plots",
      fluidPage(
        includeCSS("www/styles.css"),
        
        fluidRow(
          
          verbatimTextOutput('debug'),
          column(4,
                 
                 fluidRow(
                   h3("Grouping"),
                   
                   column(6,
                          
                          #####################
                          
                          radioButtons('group',
                                       label = 'Split plots on:',
                                       choices = GLSN,
                                       selected = 'Labels')),
                   column(6,
                          
                          radioButtons('colorize',
                                       label = 'Color on:',
                                       choices = GLSN,
                                       selected = 'Genotype')
                   )
                 ),
                 
                 fluidRow(
                   
                   # tags$hr(),
                   
                   actionButton('graphOptsBut', 'Graph Options'),
                   
                   bsModal('graphOpts', 'Graph options', 'graphOptsBut',
                           size = 'small',
                           
                           sliderInput('plheight', label = 'Plot height', ticks = FALSE,
                                       min = 200, max = 2000, value = 600, step = 200, width = '60%'),
                           
                           sliderInput('ncols', label = 'Plots per row', ticks = FALSE,
                                       min = 1, max = 6, value = 2, step = 1, width = '60%'),
                           
                           checkboxInput('logY', label = 'log y-axis',
                                         value = FALSE),
                           
                           checkboxInput('gtheme', label = 'grey background',
                                         value = FALSE),
                           
                           
                           helpText("Color scheme (http://colorbrewer2.org)"),
                           
                           uiOutput('brewing'),
                           
                           checkboxInput('customcol', label = 'Use custom color mode for cell types',
                                         value = TRUE)
                   ),
                   conditionalPanel("input.tabs1 == 'histogram'", class = 'panel_style',
                                    h3('Histogram options'),
                                    radioButtons('density', label = 'Y-axis:',
                                                 choices = c('counts', 'density'),
                                                 selected = 'density'),
                                    sliderInput('bin', label = 'Number of bins', ticks = FALSE,
                                                min = 2, max = 100, value = 40,
                                                step = 2, width = '60%')),
                   
                   
                   
                   conditionalPanel("input.tabs1 == 'scatter'",
                                    class = 'panel_style',
                                    checkboxInput('interact', label = 'Interactive plot (slower)',
                                                  value = FALSE))
                   
                 ),
                 fluidRow(
                   
                   tags$hr(),
                   
                   h3("Data selection"),
                   
                   column(6,
                          
                          radioButtons('usevp',
                                       label = 'Viewpoints',
                                       choices = c('yes', 'no'),
                                       selected = 'no'),
                          
                          uiOutput('views')
                   ),
                   
                   column(6,
                          
                          #######################
                          
                          selectInput("Yaxis",
                                      label = "Variable:",
                                      choices = c(levels(data$Type), 'Cell Number')),
                          
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
                                       class = 'actbut_style', width = '60%'),
                          
                          tags$hr(),
                          
                          h4('Tag filtering'),
                          
                          checkboxInput('SMCneighb',
                                        label = 'only SMC neighbour cells',
                                        value = FALSE)
                   )
                                    )
                 
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
                                      fluidRow(
                                        actionButton('MeanTabButton', label = 'View table',
                                                     class = 'downloadBut_style')),
                                      fluidRow(
                                        downloadButton('downloadTableMeans', 'Download as .csv',
                                                       class = "downloadBut_style")),
                                      fluidRow(uiOutput('UIgetpdf_mn')),
                                      bsModal('tabMeansMod', 'Mean ± SD', 'MeanTabButton',
                                              #tableOutput('tableMeans')),
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
