
  p <- ggplot(data = gg_data(), aes_string('Stage', 'Value', fill = input$colorize)) +
    scale_fill_manual(values = colormap(), name = NULL)
  
  if(input$group == 'Stage'){
    p <- p + facet_wrap(input$group, ncol = input$ncols, scale = 'free_x')
  } else {
    p <- p + facet_wrap(input$group, ncol = input$ncols)
  }
  
  p <- ggoptions(p, input$Yaxis, input$logY, input$gtheme)
