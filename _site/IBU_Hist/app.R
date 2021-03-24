#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("IBU Histogram"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
 
  # Libraries
  library(tidyverse)
  library(magrittr)
  library(knitr)
  library(rmarkdown)
  library(DT)
  library(mice)
  library(VIM)
  library(psych)
  library(tidyverse)
  library (readr)
  library(caret)

  # Utility function
  isEmpty <- function(column) {
    is.na(column) | column == 0 | column == "" | column == " " | column == "NA" | column == "na" | column == "Na" | column == "nA" | column == "NaN"
  }

  ### Import Data
  beer_url="https://raw.githubusercontent.com/RiccoFerraro/dds_case_study_1/main/Data/Beers.csv"
  brewery_url="https://raw.githubusercontent.com/RiccoFerraro/dds_case_study_1/main/Data/Breweries.csv"
  beer <-read_csv(url(beer_url))
  brew <-read_csv(url(brewery_url))
  # 2. Merge beer data first with the breweries data & Print first 6 and last 6 oservations in merged file.
  ### EDA: Formatting, and dealing with NA's
  # beer$Brewery_id
  # names(beer)[names(beer) == "Brewery_id"] <- "Brew_ID"
  bdat = merge(beer, brew, by.x = "Brewery_id", by.y = "Brew_ID")
  names(bdat)[names(bdat) == "Name.x"] <- "Drink_name"
  names(bdat)[names(bdat) == "Name.y"] <- "Brewery"

  bdat$State = as.factor(bdat$State)
  brewNum = bdat %>%
  filter(!is.na(bdat) & !is.na(Brewery_id)) %>%
  group_by(State) %>% summarize(unique_breweries=length(unique(Brewery_id)))

  # Plot missing data (there should be none); remove if causing error
  bdat_mice_clean <- aggr(bdat, col=c('navyblue','yellow'),
                          numbers=TRUE, sortVars=TRUE,
                          labels=names(bdat), cex.axis=.7,
                          gap=3, ylab=c("Missing Data (distribution)","Missing Data (Pattern)"))

  bdat_clean <- bdat %>% filter(bdat$Beer_ID!=2210 & bdat$Beer_ID!=1796 & bdat$Beer_ID!=1790 & bdat$Beer_ID!=2364& bdat$Beer_ID!=2322& bdat$Beer_ID!=1750& bdat$Beer_ID!=710& bdat$Beer_ID!=273& bdat$Beer_ID!=1095& bdat$Beer_ID!=963)
  imputeMissingStyle <- function(beer_id, style){
    if(beer_id=="2527"){
      style = "Scottish-Style Amber Ale"
    }
    if(beer_id=="1635"){
      style = "Lager"
    }
    style
  }
  
  # Update ABV and IBU for NA's; to hand selected data use  %>% select(Beer_ID, Brewery, Drink_name, ABV, IBU)
  bdat_hand_updated <-bdat_clean %>%
  filter(is.na(ABV) & is.na(IBU)) %>% rows_update(tibble(Beer_ID = c(2382, 2381, 1948, 1347, 2595, 1163, 940, 2490, 2489, 2488, 2487, 2472, 779, 364, 60, 59, 58, 57, 652, 2344, 2342, 1752, 61, 1724, 774, 121, 1784, 1541, 1025, 219, 307, 1096, 1056, 944, 731, 870, 869, 868, 867, 763, 504, 524, 450, 449, 448, 447, 446, 520, 506, 142, 335, 64), ABV = c(0.075, 0.055, 0.045, 0.060, 0.062, 0.0499, 0.06, 0.061, 0.038, 0.057, 0.051, 0.055, 0.05, 0.07, 0.052, 0.05, 0.049, 0.051, 0.07, 0.055, 0.049, 0.07, 0.052, 0.07, 0.058, 0.052, 0.07, 0.052, 0.086, 0.051, 0.06, 0.062, 0.055, 0.05, 0.055, 0.042, 0.05, 0.045, 0.039, 0.052, 0.072, 0.045, 0.061, 0.064, 0.058, 0.049, 0.05, 0.042, 0.05, 0.065, 0.065, 0.05), IBU = c(NA, 3.57, 65, 23, NA, 15, NA, 40, 7, 32, 35, NA, 5, NA, NA, NA, NA, NA, 80, NA, NA, NA, 28, NA, NA, 12, 40, 21, 80, NA, 38, NA, NA, 25, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 70, 17)))
  
  bdat_with_hand_updates = merge(bdat_hand_updated,bdat_clean,by="Beer_ID",all.y = TRUE) %>% rowwise %>% mutate(Brewery_id=Brewery_id.y, Drink_name=Drink_name.y, Ounces=Ounces.y, Brewery=Brewery.y, City=City.y, State=State.y, Style = imputeMissingStyle(Beer_ID, Style.y),IBU = if_else(isEmpty(IBU.y), IBU.x, IBU.y), ABV = if_else(isEmpty(ABV.y), ABV.x, ABV.y)) %>% select(Brewery_id, ABV, IBU, Drink_name, Style, Ounces, Brewery, City, State)

  # Impute IBU
  bdat.IBU.Summary <- bdat_with_hand_updates %>% filter(!isEmpty(IBU)) %>% group_by(Style) %>% summarise(median_IBU_by_style=median(IBU))
  bdat.imputed.IBU <-merge(bdat_with_hand_updates, bdat.IBU.Summary, by="Style")  %>% mutate(IBU.clean = if_else(isEmpty(IBU), median_IBU_by_style, as.double(IBU)))
  bdat.imputed.IBU.clean = bdat.imputed.IBU[,-c(4)]
  bdat.imputed.IBU.clean %>% filter(isEmpty(Style))
  
  aggr(bdat.imputed.IBU.clean, col=c('navyblue','yellow'),
       numbers=TRUE, sortVars=TRUE,
       labels=names(bdat.imputed.IBU.clean), cex.axis=.7,
       gap=3, ylab=c("Missing Data (distribution)","Missing Data (Pattern)"))

  output$distPlot <- renderPlot({
    
    x    <- bdat.imputed.IBU.clean$IBU.clean
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "International Bitterness Units",
         main = "IBU")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

