shinyServer(function(input, output, session) {
  
  #############################################
  #############################################
  
  output$views <- renderUI({
    
    if(input$usevp == 'no'){
      
      tagList(
        selectizeInput('Label',
                       label = 'Select cell types',
                       choices = levels(data$Labels), multiple = TRUE,
                       selected = 'L1 apical'),
        
        actionButton('Label_goButton', 'select all', style = actbut_style, width = '60%'))
      
    } else{
      tagList(
        
        tags$strong("Viewpoint 1"),
        textInput('vp1_name', label = NULL, value = 'vp1'),
        helpText("Select cell types"),
        
        selectizeInput('viewpoint1',
                       label = NULL, multiple = TRUE,
                       choices = levels(data$Labels), 
                       select = c('L1 apical', 'L1 basal', 'L1 basal sup', 'L1 dome')),
        
        ###
        tags$strong("Viewpoint 2"),
        textInput('vp2_name', label = NULL, value = 'vp2'),
        helpText("Select cell types"),
        
        selectizeInput('viewpoint2',
                       label = NULL, multiple = TRUE,
                       choices = levels(data$Labels),
                       select = c('L2 apical', 'L2 basal', 'L2 basal sup')),
        
        ###
        tags$strong("Viewpoint 3"),
        textInput('vp3_name', label = NULL, value = 'vp3'),
        helpText("Select cell types"),
        
        selectizeInput('viewpoint3',
                       label = NULL, multiple = TRUE,
                       choices = levels(data$Labels))
      )
    }
  })
  
  
  # do not allow overlap between viewpoint selections (would lead to probs with ggplot facet wraping):
  observe({
    
    preval <- isolate(input$viewpoint2)
    updateSelectizeInput(session, inputId = 'viewpoint2',
                         choices = levels(data$Labels)[
                           !levels(data$Labels) %in% input$viewpoint1],
                         select = preval)
  })


  observe({

    preval <- isolate(input$viewpoint3)
    updateSelectizeInput(session, inputId = 'viewpoint3',
                         choices = levels(data$Labels)[
                           !levels(data$Labels) %in% c(input$viewpoint1,input$viewpoint2)],
                         select = preval)
  })
  
  
  #############################################
  #############################################
  
  observeEvent(input$Label_goButton, {
    
    if(length(input$Label) + 1 == length(levels(data$Labels))){
      # added 1 to account for the empty "" data$Labels
      sel <- 'L1 apical'} else sel <- levels(data$Labels)
      
      updateSelectizeInput(session, inputId = 'Label',
                           selected = sel)
  })
  
  observeEvent(input$Stage_goButton, {
    
    if(length(input$Stage) == length(levels(data$Stage))){
      sel <- levels(data$Stage)[1] } else sel <- levels(data$Stage)
      
      updateCheckboxGroupInput(session, inputId = 'Stage',
                               selected = sel)
  })
  

  ####################################################
  # Create vipdata reactive table
  ####################################################
  #   add 'Viewpoints' to the raw data
  #   the vipdata() is subsetted on Types that have a viewpoint
  #   if the 'no viewpoints' option is set, then add one 'invisible' viewpoint to all the rows
  
  vipdata <- reactive({
    
    if(input$usevp == 'yes'){
      x <- viewpoint(data,
                     name_vp1 = input$vp1_name, vp1 = input$viewpoint1,
                     name_vp2 = input$vp2_name, vp2 = input$viewpoint2,
                     name_vp3 = input$vp3_name, vp3 = input$viewpoint3)
    } else {
      x <- viewpoint(data, name_vp1 = '', vp1 = input$Label,
                     name_vp2 = NULL, name_vp3 = NULL)  }
    
    # subset based on SMC neighbour tag
    if(input$SMCneighb){
      x <- x[x$neighbourSMC == 'SMC contact',]
    }
    
    return(x)
  })
  
  
  ####################################################
  # Create plotdata reactive dataset
  ####################################################
  #   depending on the currently open tab, defines the dataset to plot
  #   -- either vipdata() or the group summaries
  
  plotdata <- reactive({
    if(input$tabs1 == 'averages'){
      
      vars1 <- c("Stack", "Genotype", "Stage", "Labels")
      vars2 <- c("Genotype", "Stage", "Labels")
      
      if(input$usevp == 'yes') {
        vars1[vars1 == 'Labels'] <- "Viewpoints"
        vars2[vars2 == 'Labels'] <- "Viewpoints"
      }
      
      # Calculate number of different cells in each stack:
      filt.N.cells.stack <- countcells(data = vipdata(), vars1)
      
      # Combine with means for other traits
      summary <- groupmeans(data = vipdata(),
                            vars2 = vars2,
                            filt.N.cells.stack)
      return(summary)
    } else x <- vipdata()
    return(x)
  })
  
  
  #############################################
  # update group and colorize choices depending on type of plot
  #############################################
  
  observeEvent(plotdata(), {
    
    prevVal <- c(input$group, input$colorize)
    
    if(input$usevp == 'no') {
      prevVal[prevVal == 'Viewpoints'] <- 'Labels'
      ch <- GLSN
      
      if(input$tabs1 %in% c('averages')){
        prevVal[prevVal == 'neighbourSMC'] <- 'Labels'
        ch <- GLS}}
    
    
    if(input$usevp == 'yes') {
      ch <- GLVSN
      
      if(input$tabs1 %in% c('averages')){
        prevVal[prevVal == 'Labels'] <- 'Viewpoints'
        prevVal[prevVal == 'neighbourSMC'] <- 'Viewpoints'
        ch <- GVS}}
    
    
    updateRadioButtons(session, inputId = 'group',
                   choices = ch,
                   select = prevVal[1])
    
    updateRadioButtons(session, inputId = 'colorize',
                   choices = ch,
                   select = prevVal[2])
  })
  
  
  ####################################################
  # Create sub_plotdata reactive dataset
  ####################################################
  # subset of plot_data() values that are plotted
  # keep it separate so that color map is not changed when subsetting plots
  sub_plotdata <- reactive({
    x <- subset(plotdata(),
                Genotype %in% input$Genotype &
                  Stage %in% input$Stage &
                  Type == input$Yaxis)
    return(x)
  })
  
  
  ####################################################
  # set map for color values
  ####################################################
  
  # Define color map: custom or the one generated by 'brewery'
  colormap <- reactive({
    
    if(input$customcol & input$colorize == 'Labels'){
      
      x <- col.map <- customcolmap
      
    } else{
      
      x <- col.map(data = plotdata(),
              colorize = input$colorize,
              brew = input$brewery)
    }
    return(x)
  })
  
  #############################################
  #############################################
  
  output$Yaxis_sel <- renderUI({
    
    selectInput("Yaxis",
                label = "Variable:",
                choices = levels(plotdata()$Type))
  })
  
  #############################################
  #############################################
  
  output$brewing <- renderUI({
    
    selectizeInput('brewery', label = NULL,
                   choices = list(
                     'qualitative' = rownames(subset(
                       brewer.pal.info, category %in% 'qual' &
                         maxcolors >= length(levels(vipdata()[, input$colorize])
                         ))),
                     'diverging' = rownames(subset(
                       brewer.pal.info, category %in% 'div' &
                         maxcolors >= length(levels(vipdata()[, input$colorize])
                         ))),
                     'sequential' = rownames(subset(
                       brewer.pal.info, category %in% 'seq' &
                         maxcolors >= length(levels(vipdata()[, input$colorize])
                         )))))
  })
  
  #####################################
  ##########################################
  ##############################################
  ###################################################
  
  # get value for plot height
  plheight <- reactive(as.numeric(input$plheight))
  

  ####################################################
  ####################################################
  
  observe({

    if(input$tabs1 == 'boxplot'){

      pl_bx <- reactive({
        # Bug? Have to call colormap() here because it was not active inside ggplot function!
        colormap()
        source('prePlot.R', local = TRUE)
        p <- p + geom_boxplot(color = 'grey20', group = input$colorize)
        return(p)
      })

      output$plotBoxplot <- renderPlot(pl_bx(), height = plheight)
      output$getpdf_bx <- handle(plot = pl_bx(), width = input$getpdf_bx_width,height = input$getpdf_bx_height)
      output$UIgetpdf_bx <- renderUI(tagModal(x = 'getpdf_bx'))

    }
  })
  
  
  #############################################
  #############################################
  
  observe({
    
    if(input$tabs1 == 'scatter'){
      
      pl_jit <- reactive({
        # Bug? Have to call colormap() here because it was not active inside ggplot function!
        colormap()
        source('prePlot.R', local = TRUE)
        p <- p + geom_point(pch = 21, color =  'grey40', alpha = 0.7,
                            position = position_jitterdodge())
        return(p)
      })
      
      output$plotJitter <- renderPlot(pl_jit(), height = plheight)
      output$getpdf_jit <- handle(plot = pl_jit(), width = input$getpdf_jit_width,height = input$getpdf_jit_height)
      output$UIgetpdf_jit <- renderUI(tagModal(x = 'getpdf_jit'))
    }
  })
  
  #############################################
  #############################################
  
  observeEvent(input$interact, {
    
    output$jitter_i <- renderPlotly({
      
      source('prePlot.R', local = TRUE)
      p <- p + geom_point(aes(label = Stack), pch = 21,
                          color =  'grey40', alpha = 0.7,
                          position = position_jitterdodge())
      
      ggplotly(p, source = 'A') %>%
        layout(height = input$plheight, width = 'auto', autosize=TRUE,
               dragmode = "select", margin = list(b = 100))
    })
  })
  
  # #Not run - retrieve table with values selected on plotly
  # #would need to add key = key to aes  dodge.width = 0
  # key <- rownames(sub_plotdata())
  # output$selectTable <-  renderDataTable({
  #   d <- NULL
  #   d <- event_data("plotly_selected", source = 'A')$key
  #   if(!is.null(d)) selected <- sub_plotdata()[d,]
  # })

  #############################################
  #############################################
  
  observe({
    
    if(input$tabs1 == 'histogram'){
      
      pl_h <- reactive({
        
        # Bug? Have to call colormap() here because it was not active inside ggplot function!
        colormap()
        
        p <- ggplot(data = sub_plotdata(), aes_string('Value', fill = input$colorize)) +
          facet_wrap(input$group, ncol = input$ncols) +
          scale_fill_manual(values = colormap(), name = NULL)
        
        if(input$density == 'counts'){
          p <- p + geom_histogram(position = 'dodge', bins = input$bin)
        } else {
          p <- p + geom_histogram(aes(y = ..density..),
                                  position = 'dodge', bins = input$bin)
        }
        
        p <- ggoptions(p, input$Yaxis, input$logY, input$gtheme)
        
        return(p)
      })
      
      output$plotHist <- renderPlot(pl_h(), height = plheight)
      output$getpdf_h <- handle(plot = pl_h(), width = input$getpdf_h_width, height = input$getpdf_h_height)
      output$UIgetpdf_h <- renderUI(tagModal(x = 'getpdf_h'))
    }
  })
  
  
  #############################################
  #############################################
  
  observe({
    
    if(input$tabs1 == 'averages'){
      
      pl_mn <- reactive({
        # Bug? Have to call colormap() here because it was not active inside ggplot function!
        colormap()
        dodge <- position_dodge(width=0.3)
        
        p <- ggplot(data = sub_plotdata(), aes_string('Stage', y = 'mean',
                                                      color = input$colorize,
                                                      group = input$colorize,
                                                      fill = input$colorize)) +
          facet_wrap(input$group, ncol = input$ncols) +
          geom_line(position = dodge) +
          geom_errorbar(aes(y = mean, ymin = mean - sd, ymax = mean + sd),
                        position = dodge, width = 0.2, color = 'grey') +
          geom_point(size = rel(2), pch = 21, color = 'black', position = dodge) +
          scale_color_manual(values = colormap(), name = NULL) +
          scale_fill_manual(values = colormap(), name = NULL)
        
        p <- ggoptions(p, input$Yaxis, input$logY, input$gtheme)
        return(p)
      })
      
      output$plotMeans <- renderPlot(pl_mn(), height = plheight)
      output$getpdf_mn <- handle(plot = pl_mn(), width = input$getpdf_mn_width,height = input$getpdf_mn_height)
      output$UIgetpdf_mn <- renderUI(tagModal(x = 'getpdf_mn'))
    }
  })
  
  
  ###
  # output$means <- renderPlot({
  #   
  #   dodge <- position_dodge(width=0.3)
  #   
  #   p <- ggplot(data = sub_plotdata(), aes_string('Stage', y = 'mean',
  #                                                 color = input$colorize,
  #                                                 group = input$colorize,
  #                                                 fill = input$colorize)) +
  #     facet_wrap(input$group, ncol = input$ncols) +
  #     geom_line(position = dodge) +
  #     geom_errorbar(aes(y = mean, ymin = mean - sd, ymax = mean + sd),
  #                   position = dodge, width = 0.2, color = 'grey') +
  #     geom_point(size = rel(2), pch = 21, color = 'black', position = dodge) +
  #     scale_color_manual(values = colormap(), name = NULL) +
  #     scale_fill_manual(values = colormap(), name = NULL)
  #   
  #   ggoptions(p, input$Yaxis, input$logY, input$gtheme)
  #   
  # }, height = plheight)


  #############################################
  #############################################
  
  output$group_summary <- renderDataTable(group.summary)
  
  output$downloadMeansSD <- downloadHandler(
    filename = 'summary.csv',
    content = function(file) {write.csv2(group.summary, file, quote = FALSE, row.names = FALSE)})
  
  #############################################
  #############################################
  
  output$cell_numb_stack <- renderDataTable(N.cells.stack)
  
  output$downloadCellsStack <- downloadHandler(
    filename = 'cellsStack.csv',
    content = function(file) {write.csv2(N.cells.stack, file, quote = FALSE, row.names = FALSE)})
  
  #############################################
  #############################################
  
  output$table <- renderDataTable(data)
  
  #############################################
  #############################################
  
  output$debug <- renderPrint({
    names(input)
  })
  
})
