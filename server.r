# Server logic
server <- function(input, output) {
  
  output$pulks_table <- renderText({
    paste(input$uitrue_s)
    paste(class(input$uitrue_s))
  })
  

  output$ssPlot_R2D2 <- 
    
    renderPlot({
      # Extract data for R2D2 approach for both TV and LRV
      R2D2_plot_data <-
        R2D2_sim_data %>%
        filter(
          tau_LRV == (input$uitau_LRV * 1000),
          tau_TV  == (input$uitau_TV * 1000),
          target_LRV == (input$uitargets[1] * 1000),
          target_TV  == (input$uitargets[2] * 1000),
          acc  %in% (as.numeric(input$uitrue_s) * 1000),
          totn <= input$uissize,
          prev %in% (as.numeric(  input$uiprev) * 1000)
        ) %>%
        mutate(true_stxt = scales::percent(acc/1000,accuracy=1)) %>%
        mutate(      plot_go = 1) %>% # Plt area for GO (=1)
        mutate(plot_consider = dec_0 + dec_9) %>% # Plot area for consider
        mutate(    plot_nogo = dec_0)  # Plot area for noGO
      
      
      # Some checks
      # x <- plot_data_pulks_sens %>% 
      #   filter (
      #     tau_TV == 25,
      #     tau_LRV == 25,
      #     target_TV == 800,
      #     target_LRV == 700,
      #     prev == 200,
      #     acc == 800,
      #     totn == 100
      #   )
      # 
      
      
      
      ggplot(data = filter(R2D2_plot_data)) +
        geom_area(aes(totn, plot_go),      fill='green4') +
        geom_area(aes(totn, plot_consider), fill='blue3') +
        geom_area(aes(totn, plot_nogo),    fill='red3') +
        #geom_line(data = filter(plot_data_pulks_sens,prev==200), color='black',size=2,linetype='solid',aes(x=totn,y=plot_consider)) +
        #geom_line(data = filter(plot_data,prev==0.15), color='black',size=2,linetype='longdash',aes(x=totn,y=LRV)) +
        #scale_x_continuous('Sample size',
         #                  breaks = c(100,200,300,400,500,600),limits=c(100,600)) +
        scale_y_continuous('Decision probabilities',
                           breaks = seq(0,1,0.1)) +
        theme(
          axis.text.x = element_text(
            size=rel(2),
            angle = 90
          )
        ) +
        facet_grid(cols = vars(true_stxt))
      
      
    })  
  

}

