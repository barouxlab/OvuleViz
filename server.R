shinyServer(function(input, output, session) {
  
  
  ####################################################
  # Import segmented data
  ####################################################
  
  # open upload window on start
  observe({
    
    if(is.null(input$inputFile)){
      showModal(
        modalDialog(
          fileInput('inputFile', label = NULL),
          title = 'Upload file with segmented data',
          footer = NULL,
          fade = FALSE)
      )
    }
  })
  
  # automatically close upload window after upload
  observeEvent(input$inputFile, {
    removeModal(session)
  })
  
  # update data file
  data <- reactive({
    
    if(is.null(input$inputFile)){
      out <- read.csv2('start.csv', row.names = NULL)
      
    } else{
      out <- read.csv2(input$inputFile$datapath, row.names = NULL)
    }
    return(out)
  })
  
  ##############################################################
  # update selectors based on levels present in data()
  ##############################################################
  
  observeEvent(data(), {
    updateSelectInput(session, "Yaxis",
                      choices = c(levels(data()$Type), 'Cell Number'))
    
    updateCheckboxGroupInput(session, 'Genotype',
                             choices = levels(data()$Genotype),
                             selected = levels(data()$Genotype))
    
    updateCheckboxGroupInput(session, 'Stage',
                             choices = levels(data()$Stage),
                             selected = levels(data()$Stage))
  })
  
  #### Select all button
  
  observeEvent(input$Stage_goButton, {
    
    if(length(input$Stage) == length(levels(data()$Stage))){
      sel <- levels(data()$Stage)[1] } else sel <- levels(data()$Stage)
      
      updateCheckboxGroupInput(session, inputId = 'Stage',
                               selected = sel)
  })
  
  ##############################################################
  # Set modal for cell and viewpoint selection
  ##############################################################
  
  observeEvent(input$setCellsBut, {
    
    showModal(modalDialog(
      title = "Set viewpoints and cell labels",
      
      radioButtons('usevp',
                   label = 'Use viewpoints?',
                   choices = c('no', 'yes'), inline = TRUE,
                   selected = cellviews$usevp),
      
      uiOutput('views'),
      
      size = 'm',
      
      footer = tagList(shiny::actionButton('submitCell_But', "Apply changes",
                                           class = 'applyBut_style'),
                       shiny::modalButton("Cancel"))
    )
    )
  })
  
  # automatically close upload window after upload
  observeEvent(input$submitCell_But, {
    removeModal(session)
  })
  
  
  ##############################################################
  # Selectors for cell types / viewpoints
  ##############################################################
  
  output$views <- renderUI({
    
    if(input$usevp == 'no'){
      
      tagList(
        selectizeInput('Label',
                       label = 'Select cell types',
                       choices = levels(data()$Labels),
                       multiple = TRUE,
                       selected = cellviews$Label),
        
        actionButton('Label_goButton', 'select all',
                     class = 'actbut_style', width = '20%')
      )
      
    } else if(input$usevp == 'yes'){
      
      tagList(
        fluidRow(
          
          column(4,
                 helpText("Viewpoint name"),
                 textInput('vp1_name', label = NULL, value = cellviews$vp1_name),
                 helpText("Select cell types"),
                 
                 checkboxGroupInput('viewpoint1',
                                    label = NULL,
                                    choices = levels(data()$Labels), 
                                    select = cellviews$viewpoint1
                 )
          ),
          column(4,
                 helpText("Viewpoint name"),
                 textInput('vp2_name', label = NULL, value = cellviews$vp2_name),
                 helpText("Select cell types"),
                 
                 checkboxGroupInput('viewpoint2',
                                    label = NULL,
                                    choices = levels(data()$Labels),
                                    select = cellviews$viewpoint2
                 )
          ),
          column(4,
                 helpText("Viewpoint name"),
                 textInput('vp3_name', label = NULL, value = cellviews$vp3_name),
                 helpText("Select cell types"),
                 
                 checkboxGroupInput('viewpoint3',
                                    label = NULL,
                                    choices = levels(data()$Labels),
                                    cellviews$viewpoint3)
          )
        )
      )
    }
  })
  
  
  #### do not allow overlap between viewpoint selections
  # (would lead to probs with ggplot facet wraping):
  
  observe({
    
    c(input$submitCell_But, input$usevp)
    preval_vp1 <- input$viewpoint1
    updateCheckboxGroupInput(session, inputId = 'viewpoint1',
                             choices = levels(data()$Labels)[
                               !levels(data()$Labels) %in% c(input$viewpoint2, input$viewpoint3)],
                             select = input$viewpoint1)
  })
  
  
  observe({
    
    c(input$submitCell_But, input$usevp)
    preval_vp2 <- input$viewpoint2
    updateCheckboxGroupInput(session, inputId = 'viewpoint2',
                             choices = levels(data()$Labels)[
                               !levels(data()$Labels) %in% c(input$viewpoint1, input$viewpoint3)],
                             select = input$viewpoint2)
  })
  
  observe({
    
    c(input$submitCell_But, input$usevp)
    preval_vp3 <- input$viewpoint3
    updateCheckboxGroupInput(session, inputId = 'viewpoint3',
                             choices = levels(data()$Labels)[
                               !levels(data()$Labels) %in% c(input$viewpoint1, input$viewpoint2)],
                             select = input$viewpoint3)
  })
  
  
  #### Select all button
  
  observeEvent(input$Label_goButton, {
    
    if(length(input$Label) + 1 == length(levels(data()$Labels))){
      # added 1 to account for the empty "" data()$Labels
      sel <- 'L1 apical'} else sel <- levels(data()$Labels)
      
      updateSelectizeInput(session, inputId = 'Label',
                           selected = sel)
  })
  
  
  #### Update cellviews reactive according to cell type / viewpoint electors
  
  observeEvent(input$submitCell_But, {
    
    cellviews$usevp <- input$usevp
    cellviews$Label <- input$Label
    cellviews$vp1_name <- input$vp1_name
    cellviews$viewpoint1 <- input$viewpoint1
    cellviews$vp2_name <- input$vp2_name
    cellviews$viewpoint2 <- input$viewpoint2
    cellviews$vp3_name <- input$vp3_name
    cellviews$viewpoint3 <- input$viewpoint3
    
  })
  
  observeEvent(input$brewery,{
    cellviews$brewery <- input$brewery
  })
  
  ##############################################################
  # update group and colorize choices depending on viewpoint use
  ##############################################################
  
  observe({
    
    prevVal1 <- input$group
    prevVal2 <- input$colorize
    
    if(cellviews$usevp == 'no') {
      prevVal1[prevVal1 == 'Viewpoints'] <- 'Labels'
      prevVal2[prevVal2 == 'Viewpoints'] <- 'Labels'
      ch_Group <- GLSNS
      ch_Color <- GLSN
    }
    
    if(cellviews$usevp == 'yes') {
      ch_Group <- GLVSNS
      ch_Color <- GLVSN
    }
    
    updateRadioButtons(session, inputId = 'group',
                       choices = ch_Group,
                       select = prevVal1)
    
    updateRadioButtons(session, inputId = 'colorize',
                       choices = ch_Color,
                       select = prevVal2)
  })
  
  
  ##############################################################
  # Create vipdata reactive table
  ##############################################################
  
  # Add 'Viewpoints'.
  # If the 'no viewpoints' option is set, add
  # one invisible viewpoint to all the rows.
  
  vipdata <- reactive({
    
    if(cellviews$usevp == 'yes'){
      
      x <- viewpoint(data(),
                     name_vp1 = cellviews$vp1_name, vp1 = cellviews$viewpoint1,
                     name_vp2 = cellviews$vp2_name, vp2 = cellviews$viewpoint2,
                     name_vp3 = cellviews$vp3_name, vp3 = cellviews$viewpoint3)
      
    } else if(cellviews$usevp == 'no'){
      
      x <- viewpoint(data(), name_vp1 = '', vp1 = cellviews$Label,
                     name_vp2 = NULL, name_vp3 = NULL)
    }
    
    return(x)
  })
  
  ##############################################################
  # Create subsetdata reactive dataset
  ##############################################################
  
  # subset vipdata based on the selected genotype, stage and tags
  
  subsetdata <- reactive({
    
    x <- subset(vipdata(),
                Genotype %in% input$Genotype &
                  Stage %in% input$Stage)
    
    if(input$SMCneighb){
      x <- x[x$neighbourSMC == 'SMC contact',]
    }
    
    return(x)
    
  })
  
  
  ##############################################################
  # Create plotdata reactive dataset
  ##############################################################
  
  # depending on the y-axis variable chosen, defines the dataset to plot
  # (i.e. Cell number or any of the others)
  
  plotdata <- reactive({
    
    if(input$Yaxis == 'Cell Number'){
      
      # Calculate number of different cells in each stack:
      vars1 <- c("Stack", "Stage", input$colorize, input$group)
      
      filt.N.cells.stack <- countcells(data = subsetdata(), vars1)
      
      x <- cbind(Type = 'Cell Number', filt.N.cells.stack)
      names(x)[names(x) == 'N'] <- 'Value'
      
    } else{
      x <- subsetdata()
    }
    
    return(x)
  })
  
  ##############################################################
  # Create gg_data reactive dataset
  ##############################################################
  
  # subset of plotdata() values that are plotted
  # inherited from previous versions, maybe not required anymore
  
  gg_data <- reactive({
    
    x <- subset(plotdata(),
                # Genotype %in% input$Genotype &
                #   Stage %in% input$Stage &
                Type == input$Yaxis)
    
    return(x)
  })
  
  
  ##############################################################
  # Set map for color values
  ##############################################################
  
  # Define color map: custom or the one generated by 'brewery'
  colormap <- reactive({
    
    if(input$customcol & input$colorize == 'Labels'){
      x <- customcolmap
      
    } else{
      
      x <- col.map(data = plotdata(),
                   colorize = input$colorize,
                   brew = cellviews$brewery)
    }
    return(x)
  })
  
  ##############################################################
  # Update color scheme choices depending on colorize factor
  ##############################################################
  
  observe({
    
    if(input$customcol & input$colorize == 'Labels'){
      
      # do nothing while using Labels with custom colors
      
    } else {
      
      # list available brews
      
      available_brews <- list(
        'qualitative' = rownames(
          subset(brewer.pal.info,
                 category %in% 'qual' &
                   maxcolors >= length(levels(subsetdata()[, input$colorize]))
          )
        ),
        'sequential' = rownames(
          subset(brewer.pal.info,
                 category %in% 'seq' &
                   maxcolors >= length(levels(subsetdata()[, input$colorize]))
          )
        )
      )
      
      # set selected value
      if(cellviews$brewery %in% unlist(available_brews)){
        
        # keep last used color scheme when available
        precolval <- cellviews$brewery
        
      } else {
        
        # use the first available color scheme when previous is not available
        precolval <- unlist(available_brews)[1]
      }
      
      updateSelectizeInput(session, 'brewery',
                           choices = available_brews,
                           selected = precolval)
    }
  })
  
  
  ##############################################################
  ##############################################################
  ##############################################################
  
  
  
  ##############################################################
  # Set value for plot height
  ##############################################################
  
  plheight <- reactive(as.numeric(input$plheight))
  
  
  ##############################################################
  # Boxplot tab
  ##############################################################
  
  observe({
    
    if(input$tabs1 == 'boxplot'){
      
      pl_bx <- reactive({
        # Bug? Have to call colormap() here because it was not active inside ggplot function
        colormap()
        source('prePlot.R', local = TRUE)
        p <- p + geom_boxplot(color = 'grey20', group = input$colorize)
        
        if(input$showPval){
          p <- addANOVA(p, gg_data(), input$group, input$colorize)
        }
        
        return(p)
      })
      
      output$plotBoxplot <- renderPlot(pl_bx(), height = plheight)
      output$getpdf_bx <- handle(plot = pl_bx(), width = input$getpdf_bx_width,height = input$getpdf_bx_height)
      output$UIgetpdf_bx <- renderUI(tagModal(x = 'getpdf_bx'))
      
    }
  })
  
  
  ##############################################################
  # Scatterplot tab
  ##############################################################
  
  observe({
    
    if(input$tabs1 == 'scatter'){
      
      pl_jit <- reactive({
        # Bug? Have to call colormap() here because it was not active inside ggplot function!
        colormap()
        source('prePlot.R', local = TRUE)
        p <- p + geom_point(pch = 21, color =  'grey40', alpha = 0.7,
                            position = position_jitterdodge())
        
        if(input$showPval){
          p <- addANOVA(p, gg_data(), input$group, input$colorize)
        }
        
        return(p)
      })
      
      output$plotJitter <- renderPlot(pl_jit(), height = plheight)
      output$getpdf_jit <- handle(plot = pl_jit(), width = input$getpdf_jit_width,height = input$getpdf_jit_height)
      output$UIgetpdf_jit <- renderUI(tagModal(x = 'getpdf_jit'))
    }
  })
  
  ##############################################################
  # Interactive plotly plot
  ##############################################################
  
  observeEvent(input$interact, {
    
    output$jitter_i <- renderPlotly({
      
      source('prePlot.R', local = TRUE)
      p <- p + geom_point(aes(label = Stack), pch = 21,
                          color =  'grey40', alpha = 0.7,
                          position = position_jitterdodge())
      
      ggplotly(p, source = 'A') %>%
        layout(height = input$plheight, width = 'auto', autosize=TRUE,
               dragmode = "select", margin = list(b = 100))
    }
    )
  })
  
  ##############################################################
  # Histogram tab
  ##############################################################
  
  observe({
    
    if(input$tabs1 == 'histogram'){
      
      pl_h <- reactive({
        
        p <- ggplot(data = gg_data(), aes_string('Value', fill = input$colorize)) +
          facet_wrap(input$group, ncol = input$ncols) +
          scale_fill_manual(values = colormap(), name = NULL)
        
        
        if(input$density == 'counts'){
          p <- p + geom_histogram(position = 'dodge', bins = input$bin)
        } else {
          p <- p + geom_histogram(aes(y = ..density..),
                                  position = 'dodge', bins = input$bin)
        }
        
        p <- ggoptions(p, input$Yaxis, input$logY, input$gtheme)
        
        if(input$showPval){
          p <- addANOVA(p, gg_data(), input$group, input$colorize)
        }
        
        return(p)
      })
      
      output$plotHist <- renderPlot(pl_h(), height = plheight)
      output$getpdf_h <- handle(plot = pl_h(), width = input$getpdf_h_width, height = input$getpdf_h_height)
      output$UIgetpdf_h <- renderUI(tagModal(x = 'getpdf_h'))
    }
  })
  
  
  ##############################################################
  # Means tab
  ##############################################################
  
  observe({
    
    if(input$tabs1 == 'means'){
      
      vars <- c("Stage", input$colorize, input$group)
      
      av_data <- reactive({
        ddply(gg_data(), vars, summarise,
              mean = mean(Value),
              sd   = sem(Value),
              n = length(Value))
      })
      
      pl_mn <- reactive({
        
        dodge <- position_dodge(width=0.3)
        
        p <- ggplot(data = av_data(), aes_string('Stage', y = 'mean',
                                                 color = input$colorize,
                                                 group = input$colorize,
                                                 fill = input$colorize)) +
          facet_wrap(input$group, ncol = input$ncols) +
          geom_errorbar(aes(y = mean, ymin = mean - sd, ymax = mean + sd),
                        position = dodge, width = 0.2, color = 'grey') +
          geom_line(position = dodge) +
          geom_point(size = rel(4), pch = 21, color = 'black', position = dodge) +
          scale_color_manual(values = colormap(), name = NULL) +
          scale_fill_manual(values = colormap(), name = NULL)
        
        if(input$manualY){
          p <- p + coord_cartesian(ylim = c(as.numeric(input$minY),
                                            as.numeric(input$maxY))
          )
        }
        
        p <- ggoptions(p, input$Yaxis, input$logY, input$gtheme)
        
        if(input$showPval){
          p <- addANOVA(p, gg_data(), input$group, input$colorize)
        }
        
        return(p)
        
      })
      
      #######
      
      output$tableMeans <- renderDataTable(av_data())
      output$downloadTableMeans <- downloadHandler(
        filename = 'means.csv',
        content = function(file) {write.csv2(av_data(), file, quote = FALSE, row.names = FALSE)})
      
      output$plotMeans <- renderPlot(pl_mn(), height = plheight)
      output$getpdf_mn <- handle(plot = pl_mn(), width = input$getpdf_mn_width,height = input$getpdf_mn_height)
      output$UIgetpdf_mn <- renderUI(tagModal(x = 'getpdf_mn'))
    }
  })
  
  ##############################################################
  # gg_data table tab
  ##############################################################
  
  output$download_gg_data <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {write.csv2(gg_data(), file, quote = FALSE, row.names = FALSE)})
  
  output$gg_data_table <- renderDataTable(gg_data())
  
  
  ##############################################################
  # Cell number table tab
  ##############################################################
  
  output$cell_numb_stack <- renderDataTable(
    
    ddply(data()[data()$Type == "Cell Volume", c("Genotype", "Stage", "Stack")],
          c("Genotype", "Stage"),
          summarise,
          Number_stacks = length(unique(Stack)))
  )
  
  ##############################################################
  # All data tab
  ##############################################################
  
  output$alldata <- renderDataTable(data())
  
  ##############################################################
  # Debug tab
  ##############################################################
  
  output$debug <- renderPrint({
    
  })
})
