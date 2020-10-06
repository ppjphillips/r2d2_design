# Global code run at app start only 
library(tidyverse)
library(haven)
library(scales)
library(knitr)
library(shiny)

#R2D2_path <- "C:/Users/ppjph/OneDrive - University of California, San Francisco/Other_Projects/R2D2/Analysis/2020_10_Design/data/"
R2D2_path <- "https://github.com/ppjphillips/r2d2_design/raw/main/"

R2D2_sim_data <- read_dta(str_c(R2D2_path,"all_data4R_2020_10_05.dta"))
  

# Read in data and round to 3 dp to avoid floating error
#sens_data <- read_csv(str_c(one_fend_path,"sens_data.csv")) %>%
#  mutate_at(c("tau","prev","true_s","target"),~round(.,3))


# Create input object for testing 
input <- list(
  uitargets = c(0.7,0.8),
  uitau_fend = 0.8,
  uitau_LRV = 0.025,
  uitau_TV = 0.025,
  ussize = 500,
  uiprev = 0.2,
  uitrue_s = c(0.7,0.8,0.85,0.9,0.95)
)


# Define UI
ui <- fixedPage(
  
  titlePanel("Sample size calculations for R2D2"),


  sidebarLayout(
    
    sidebarPanel(
      h3("Target Sensitivity"),
      
      #p("WHO TPP Target: 90%"),
                 
      sliderInput(
        "uitargets", label = "Target Value (right) and Minimum Acceptable Value (left):",
        min = 0.5, max = 0.95, value = c(0.7,0.8), ticks = FALSE, step = 0.05
      ), 
      
      h3("Acceptable Risks"),

      sliderInput(
        "uitau_LRV", label = "False GO:",
        min = 0, max = 0.25, value = 0.1, ticks=FALSE, step = 0.025
      ),
      sliderInput(
        "uitau_TV", label = "False no-GO:",
        min = 0, max = 0.25, value = 0.1, ticks=FALSE, step = 0.025
      ),  
      
      h3("Sample size"),
      
      sliderInput(
        "uissize", label = "Maximum for plotting:",
        min = 100, max = 1000, value = 500, ticks=FALSE, step = 100
      ),  
      
      h3("Prevalence"),

      sliderInput(
        "uiprev", label = "Prevalence of disease:",
        min = 0.1, max = 0.25, value = 0.2, ticks=FALSE, step = 0.05
      ),  
      
      width = 3
    ),
    
    mainPanel(
     
      checkboxGroupInput(
        inputId = "uitrue_s", 
        label = "Select which true accuracy values to plot:",
        choices = c("60%" = 0.6, "65%" = 0.65, 
                    "70%" = 0.7, "75%" = 0.75,
                    "80%" = 0.8, "85%" = 0.85,
                    "90%" = 0.9, "92%" = 0.92, 
                    "95%" = 0.95, "98%" = 0.98),
        selected = c(0.7,0.8,0.9,0.95),
        inline = TRUE
      ),
      
      plotOutput(outputId = "ssPlot_R2D2")  
    )      
  ) 
)

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
      R2D2_plot_data
      
      
      dec_colors <- c('No-Go'='red2','Go'='green2','Consider'='blue2')
      
      ggplot(data = filter(R2D2_plot_data)) +
        geom_area(aes(totn, plot_go,       fill="Go"),       alpha=0.8) +
        geom_area(aes(totn, plot_consider, fill="Consider"), alpha=0.8) +
        geom_area(aes(totn, plot_nogo,     fill="No-Go"),    alpha=0.8) +
        geom_line(aes(totn,0.5), linetype = "dashed") +
        geom_line(aes(totn,0.9), linetype = "dashed") +
        geom_line(aes(totn,0.1), linetype = "dashed") +
        scale_fill_manual(values = dec_colors) +
        facet_grid(~true_stxt) +
        theme_gray(base_size=15) +
        scale_y_continuous('Decision probabilities',
                           breaks = seq(0,1,0.1)) +
        scale_x_continuous('Sample size') + 
        labs(fill = "Decision:") +
        theme(legend.position="bottom") +
        theme(axis.text.x = element_text(angle = 90))
      
      
      
    })  
  

}


# Complete app with UI and server components
shinyApp(ui, server)

