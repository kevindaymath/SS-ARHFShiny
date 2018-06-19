library(shiny)
library(DT)
library(leaflet)
library(datasets)
# library(spdep)
# library(devtools)
# library(maptools)
# library(rgdal)
# library(rmapshaper)
# library(rgeos)
# arhf = read.csv("//dir-nas/bcbb/KDsummerproj/Data/ARHF.csv")
# states = read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USStateOrder.csv")
# states <- states[order(states$NAME),] #order data via state name
# countyRegionList=read.csv("//dir-nas/bcbb/KDsummerproj/Data/CountyRegionList.csv")
# countyRegionList=countyRegionList[order(countyRegionList$NAME),] #order data via county name
# orderRe=read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USRegionOrder.csv")
# mapRegion=readShapePoly("//dir-nas/bcbb/KDsummerproj/QGIS/SScountyUS.shp")
# simplified <- rmapshaper::ms_simplify(mapRegion)
# coordinates <- coordinates(mapRegion)
# orderRe <- cbind(orderRe,coordinates)
# orderCo=read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USCountyOrderPR1.csv")
# mapCounties=readShapePoly("//dir-nas/bcbb/KDsummerproj/QGIS/SScountyUSPR1.shp")
# simplifiedCo <- gSimplify(mapCounties,tol=0.001)
# coordinates <- coordinates(mapCounties)
# orderCo <- cbind(orderCo,coordinates)
# SSCountyPar=read.csv("//dir-nas/bcbb/KDsummerproj/Data/SSCountyParticipantNumbers.csv")
# SSRegionPar=read.csv("//dir-nas/bcbb/KDsummerproj/Data/SSRegionParticipantNumbers.csv")
# SSRegionPar <- SSRegionPar[order(SSRegionPar$RegionOrder),]
# countyRegionList <- countyRegionList[order(countyRegionList$GEOID),]
# countyRegionList["Participants"] <- SSCountyPar$freq
# newnames <- c("Water Area in Square Miles (2010)","Farms - Number (2002)","Farmland as a % of Total Land (2002)",
#               "Population Density per Square Mile (2010)","% Good Air Quality Days (2011)","Daily Fine Particulate Matter (2010)",
#               "Days w/8-hr Avg Ozone ovr NAAQS (2010)",	"# Dsgntd Txc Site Not Undr Cntrl (2012)","Total Number Hospitals (2010)",
#               "STG Hosp w/Breast Cancer Scrn/Mam (2010)","Census Population (2010)","Percent Black/African American Population (2010)",
#               "Medicaid Eligibles, Total (2008)","% < 65 without Health Insurance (2010)","Median Household Income (2010)",
#               "% Persons in Poverty (2010)","Percent Urban Population (2010)","Percent Urban Housing Units (2010)",
#               "% Persons 25+ W/4+ Yrs College (2006-10)","% Agric/Forest/Fish/Hunt/Mine Workers (2006-10)",	"Total Births (2010)",
#               "Total Deaths (2010)")
# names(arhf)[c(2:23)] <- newnames
# arhf <- arhf[-c(6,9)]
# mapVars = read.csv("//dir-nas/bcbb/KDsummerproj/Data/USRegionVariables.csv")
# a <- colnames ( mapVars[,grepl("X..", colnames(mapVars))] )
# newnames2 <- c("African American", "Medicaid Eligibles", "< 65 W/o Health Insurance",
#               "Persons in Poverty", "Urban Population", "High Education", "Workers")
# names(mapVars)[match(a,names(mapVars))] <- newnames2
# # load("//dir-nas/bcbb/KDsummerproj/Code/MapRegionsNoPR.RData")
# ranks <- data.frame(lapply(arhf,rank))
# ranks[,1] <- NA
# regionLevelInfo = read.csv("//dir-nas/bcbb/KDsummerproj/Data/RegionLevelInfo.csv")[,-1]
# newnames3 <- c("Region Name","Land Area in Square Miles (2010)","Water Area in Square Miles (2010)","Farms - Number (2002)",
#                "Farmland as a % of Total Land (2002)", "Population Density per Square Mile (2010)","Average Daily Fine Particulate Matter (2010)",
#               "Average Days w/8-hr Avg Ozone ovr NAAQS (2010)",	"Total Number Hospitals (2010)",
#               "STG Hosp w/Breast Cancer Scrn/Mam (2010)","Census Population (2010)","Percent Black/African American Population (2010)",
#               "Medicaid Eligibles, Total (2008)","% < 65 without Health Insurance (2010)","Average Median Household Income (2010)",
#               "% Persons in Poverty (2010)","Percent Urban Population (2010)", "% Persons 25+ W/4+ Yrs College (2006-10)",
#               "% Agric/Forest/Fish/Hunt/Mine Workers (2006-10)",	"Total Births (2010)", "Total Deaths (2010)")
# names(regionLevelInfo)<- newnames3
# ranksRegion <- data.frame(lapply(regionLevelInfo,rank))
# ranksRegion[,1] <- NA
# mapVars <- mapVars[order(mapVars$Region.Name),]
# regionLevelInfo <- cbind(regionLevelInfo,mapVars$Region.Order)
# regionLevelInfo <- regionLevelInfo[order(regionLevelInfo$`mapVars$Region.Order`),]

load("data\\data.RData") # Loads the workspace of the variables created by the comments above
USPHDInfo = read.csv("data\\USPHDINFO(Condensed).csv")[,-1]

function(input, output) {
  
  chosenState <- reactive({
    input$state
  })

  
  values <- reactiveValues()
  
  output$countySelector <- renderUI({
    stateabv <<-states[which(states$NAME==chosenState()),2]
    counties <<- countyRegionList[countyRegionList$STUSPS==stateabv,]
    selectInput("county","Choose which county:",
                choices=counties$NAME)
  })
  
  chosenCounty <- reactive({
    input$county
  })
  
  output$SSInfo <- renderText({
    paste("The Sister Study enrolled over 50 thousand women with sisters that were diagnosed with breast cancer",
          "between 2003 and 2009 in the US and Puerto Rico. This app draws potential health covariates",
          "from Area Health Resources Files (AHRF) and groups counties to corresponding health regions. It also",
          "creates maps of the regions and Sister Study participant numbers.",
          "More information can be found at: https://sisterstudy.niehs.nih.gov")
  })



  output$region <- renderText({
    region <<- counties[counties$NAME==chosenCounty(),8]
    string <<- paste(chosenCounty(), "is in Region",as.character(region))
    print(string)
  })


  
  output$Info <- renderDataTable({
    fips <<-  counties[which(counties$NAME==chosenCounty()),4]
    z <- as.data.frame(t(arhf[which(arhf$FIPS==fips),]))
    z <- cbind(z,t(ranks[which(arhf$FIPS==fips),]))
    datatable(z,options = list(pageLength = -1,dom = 't'),colnames=(c("Total/%", "County Rank")))

  })
  
  output$RegionInfo <- renderDataTable({
    region <<- counties[counties$NAME==chosenCounty(),8]
    countyNames <-  countyRegionList[which(countyRegionList$PHDnum==as.character(region)),c(4,5,9)]
    z <- countyNames
    datatable(z,rownames = FALSE, options = list(pageLength = -1,dom = 't'))
    
  })
  
  output$regiontotal <- renderText({
    region <<- counties[counties$NAME==chosenCounty(),8]
    countyNames <-  countyRegionList[which(countyRegionList$PHDnum==as.character(region)),c(4,5,9)]
    z <- countyNames
    string <<- paste(as.character(region), "has",sum(z[,3]),"participants")
    print(string)
  })
  
  output$RegionStat <- renderDataTable({
    region <<- counties[counties$NAME==chosenCounty(),8]
    z <- as.data.frame(t(regionLevelInfo[which(regionLevelInfo$`Region Name`==as.character(region)),c(1:21)]))
    z <- cbind(z,t(ranksRegion[which(regionLevelInfo$`Region Name`==as.character(region)),]))
    datatable(z,options = list(pageLength = -1,dom = 't'),colnames=(c("Total/%", "Region Rank")))
    
  })
  
  
  output$Map <- renderLeaflet({
    region <<- as.character(counties[counties$NAME==chosenCounty(),8])
    rnum <- which(orderRe$NAME==as.character(region))
    fips <<-  counties[which(counties$NAME==chosenCounty()),4]
    cnum <- which(orderCo$GEOID==fips)
    leaflet() %>% 
      # addTiles() %>% 
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = simplified, weight = .8,color="blue", label= ~NAME) %>%
      # addPolygons(data = simplifiedCo, weight = .4,color="red") %>%
      addMarkers(lng=orderCo[cnum,8], lat=orderCo[cnum,9], popup=chosenCounty())%>%
      setView(orderRe[rnum,4],orderRe[rnum,5],zoom=6) 
  }) #END RENDERLEAFLET OUTPUT
  
  output$removal <- renderText({
    paste("521 of the 50,844 participants were removed out of the region count due to missing counties of residence.")
  })
  
  output$FullMap <- renderLeaflet({
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = SSRegionPar$freq, bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g participants", SSRegionPar$RegionName, 
                      SSRegionPar$freq) %>% lapply(htmltools::HTML)
    m <- leaflet(SSRegionPar) %>%
      setView(-96, 38, 4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      
      addPolygons(data = simplified, fillColor = ~pal(SSRegionPar$freq), weight = 2, opacity = 1, 
                  color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", 
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  chosenVariable <- reactive({
    input$variable
  })
  
  max<- reactive({
    input$range[2]
  })
  min<- reactive({
    input$range[1]
  })
  
  
  output$PercentMap <- renderLeaflet({
    names(mapVars)[names(mapVars) == "Workers"] <- "Agric. Etc. Workers"
    # bins <- c(0,25,50,75,100)
    quart <- (max()-min())/4
    bins <- c(min(), min()+quart, min()+2*quart,min()+3*quart, max())
    pal <- colorBin("YlOrRd", domain = mapVars[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g %%", mapVars$Region.Name, mapVars[,chosenVariable()]
                      ) %>% lapply(htmltools::HTML)
    m <- leaflet(mapVars) %>%
      setView(-96, 38, 4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      
      addPolygons(data = simpNoPR, fillColor = ~pal(mapVars[,chosenVariable()]), weight = 2, opacity = 1, 
                  color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", 
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  chosenhospital <- reactive({
    input$hospital
  })
  
  output$hospitalMap <- renderLeaflet({
    if(chosenhospital()=="Total Number Hospitals (2010)") {
      bins <- c(0, 3, 6, 12, 21, 30, 75, 100, Inf)
    }
    else {
      bins <- c(0, 2, 4, 6, 10, 15, 20, 35, Inf)
    }
    pal <- colorBin("YlOrRd", domain = regionLevelInfo[,chosenhospital()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g hospitals", regionLevelInfo$`Region Name`, 
                      regionLevelInfo[,chosenhospital()]) %>% lapply(htmltools::HTML)
    m <- leaflet(regionLevelInfo) %>%
      setView(-96, 38, 4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      
      addPolygons(data = simpNoPR, fillColor = ~pal(regionLevelInfo[,chosenhospital()]), weight = 2, opacity = 1, 
                  color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", 
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  output$statephd  <- renderDataTable({
    z <- as.data.frame(t(USPHDInfo[which(USPHDInfo$NAME==chosenState()),]))
   
    datatable(z,options = list(pageLength = -1,dom = 't'))
    
  })
  output$decisions <- renderText({
    paste("",
          ifelse(chosenState()=="Massachusetts",
          "- Western: Berkshire, Franklin, Hampshire, and Hampden\n
          - Central: Worchester\n
          - Metro west: Norfolk and Middlesex\n
          - Northeast: essex\n
          - Boston: Suffolk\n
          - Southeast: Bristol, Plymouth, and barnstable",
          ifelse(chosenState()=="Arizona","Maricopa its own region, all of Yavapai in NAHEC and all of Pinal in EAHEC.",
          ifelse(chosenState()=="Connecticut",
                "- SW: Fairfield and New Haven\n 
                 - Health360: Litchfield\n 
                 - Central: Hartford, Middlesex, and Tolland\n 
                 - Eastern: Windham and New London","")))
    )
  })
  
}
