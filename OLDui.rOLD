# Global code run at app start only 
library(tidyverse)
library(haven)
library(scales)
library(knitr)
library(shiny)

R2D2_path <- "C:/Users/ppjph/OneDrive - University of California, San Francisco/Other_Projects/R2D2/Analysis/2020_10_Design/data/"

#R2D2_sim_data <- read_dta(str_c(R2D2_path,"all_data4R_2020_10_05.dta"))
R2D2_sim_data <- read_dta(all_data4R_2020_10_05.dta)
  

# Read in data and round to 3 dp to avoid floating error
#sens_data <- read_csv(str_c(one_fend_path,"sens_data.csv")) %>%
#  mutate_at(c("tau","prev","true_s","target"),~round(.,3))


# Create input object for testing 
input <- list(
  uitargets = c(0.7,0.8),
  uitau_fend = 0.8,
  uitau_LRV = 0.025,
  uitau_TV = 0.025,
  uiprev = 0.2,
  uitrue_s = c(0.7,0.8,0.85,0.9,0.95)
)


# Define UI
ui <- fixedPage(
  
  titlePanel("Sample size calculations for R2D2"),


  sidebarLayout(
    
    sidebarPanel(
      h3("Targets"),
      
      #p("WHO TPP Target: 90%"),
                 
      sliderInput(
        "uitargets", label = "Select the Target Value (right slider) and Minimum Acceptable Value (left slider):",
        min = 0.5, max = 0.95, value = c(0.7,0.8), ticks = FALSE, step = 0.05
      ), 
      
      h3("Acceptable Risks"),

      sliderInput(
        "uitau_LRV", label = "False GO:",
        min = 0, max = 0.2, value = 0.1, ticks=TRUE, step = 0.025
      ),
      sliderInput(
        "uitau_TV", label = "False no-GO:",
        min = 0, max = 0.2, value = 0.1, ticks=TRUE, step = 0.025
      ),  
      
      h3("Sample size"),
      
      sliderInput(
        "uissize", label = "Maximum for plotting:",
        min = 100, max = 1000, value = 1000, ticks=FALSE, step = 100
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
