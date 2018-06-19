library(shiny)
library(leaflet)
library(DT)
states = read.csv("data\\USStateOrder.csv")
states <- states[order(states$NAME),] #order data via region name
newnames2 <- c("African American", "Medicaid Eligibles", "< 65 W/o Health Insurance",
              "Persons in Poverty", "Urban Population", "High Education", "Agric. Etc. Workers")

# Define UI for dataset viewer application
fluidPage(
  
  # Application title
  titlePanel("County vs. Health Regions with AHRF and SS Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a state and county below"),
      selectInput("state",label="Choose a state",
                  choices=states$NAME, selected="Alabama"),
      br(),
      uiOutput("countySelector"),
      br(),
      textOutput("SSInfo")
    ),


    mainPanel(
      verbatimTextOutput("region"),
      tabsetPanel(
        tabPanel("County Statistics", DT::dataTableOutput("Info")), 
        tabPanel("List of Counties in Same Region", DT::dataTableOutput("RegionInfo"),verbatimTextOutput("regiontotal")),
        tabPanel("Region Statistics", DT::dataTableOutput("RegionStat")),
        tabPanel("Map with Selected County", leafletOutput("Map")),
        tabPanel("SS Participant # Map", leafletOutput("FullMap"),textOutput("removal")),
        tabPanel("Percent Variables Map", selectInput("variable",label="Choose a Variable",
                                     choices=newnames2, selected="African American"),
                 sliderInput("range",label="Range of Interest(%):",min=0,max=100,value=c(0,100)),
                 leafletOutput("PercentMap")),
        tabPanel("Hospital Map", selectInput("hospital",label="Choose a Hospital",
                                                      choices=c("Total Number Hospitals (2010)","STG Hosp w/Breast Cancer Scrn/Mam (2010)"), 
                                                      selected="Total Number Hospitals (2010)"),leafletOutput("hospitalMap")),
        tabPanel("State Division Info",DT::dataTableOutput("statephd"),textOutput("decisions"))
      )
      
    )
)

)