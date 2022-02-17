library(readxl, warn.conflicts = FALSE)
library(leaflet, warn.conflicts = FALSE)
library(geojsonio, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(htmltools, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(rsconnect, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(ggformula, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)   # for interactive visuals
library(magrittr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(hashmap, warn.conflicts = FALSE)
#deployApp()

## Get today's year and create a copyright string
yr <- year(Sys.Date())
copyRight <- paste(year(Sys.Date()), 
                     "&copy Chang, Huandong, Goswami, Vidush, Qin, Yi - Grinnell College")

## import cleaned data (refer to data cleaning code in separate file)
terror <- read_csv("terror_trim.csv") %>%
  mutate(weaptype1_txt = ifelse(weaptype1_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)",
                                substr(weaptype1_txt, 1, 7), weaptype1_txt))

## All countries whose data are obtainable
obtainable <- levels(as.factor(terror$country_txt))

## define marker colors for addCircleMarkers
terror$color <- ifelse(terror$success == 1, "green", "red") 
colors = c("chartreuse","gold","firebrick", "dodgerblue", "hotpink", "darkviolet", "darkorange","aquamarine", "black")
hash <- hashmap(levels(as.factor(terror$attacktype1_txt)), colors)    
terror$color = hash[[terror$attacktype1_txt]]


## Constant values
ANIMATION_SPEED <- 1500
MIN <- min(terror$iyear)
MAX <- max(terror$iyear)

## Important WorldCountry shapes
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))
    ),
  
  headerPanel(
    h1("Global Terror", 
       style = "font-family: 'Lobster', cursive;
       font-weight: 1000; line-height: 1.1; 
       color: #4d3a7d;")),
  
  ## ----- Define tabs ------- ##
  tabsetPanel(
    id = "tab_being_displayed", # define an ID for the panel

    ## ----- First tab: line plots with variable selections -----##
    tabPanel("Plots", 
             # Main panel for displaying outputs ----
             
             # Output: Histogram ----
             mainPanel(plotOutput(outputId = "distPlot"),
                       textOutput("selected_var"),
                       list(ui = fluidPage(
                         uiOutput("tab")
                       ))
             ),
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               textOutput("Instruction"),
               selectInput(inputId = "region",
                           label = "Region",
                           choices=unique(terror$region_txt),
                           selected = "South Asia"),
               
               
               selectInput(inputId = "country",
                           label = "Country",
                           choices = NULL),
               
               selectInput(inputId = "y",
                           label = "Y-Axis",
                           choices=c("Count", "Success_rate")),
               
               selectInput(inputId = "color",
                           label = "Color By",
                           choices=c("Attacktype","Weaptype","Targtype"),
                           selected = "attacktype1_txt",
                           multiple=FALSE
               )
               
             )
    ),
    
    ## ----- Second tab: map and pie chart ----- ##
    tabPanel("Map and pie chart", 
             
             ## Enable checkbox selection
             checkboxInput("markers", "Show attack instances"),
             
             ## Output: pie chart (wrapped in a div to customize position)
             tags$div(plotOutput(outputId = "distPie"), 
                      style="position:absolute; left:64%; top:30%; width:900px; height:500px; "),
             ## Output: Leaflet map 
             leafletOutput("map", width = "1000px", height = "500px"),
              
             


             
             ## Slider for "year" variable
             absolutePanel(top = 10, right = 10,
                           sliderInput(inputId = "yr",
                                       label = "Year",
                                       min = MIN,
                                       max = MAX,
                                       value = MIN,
                                       step = 2,
                                       width = '600px',
                                       animate = animationOptions(interval = ANIMATION_SPEED,
                                                                  playButton = icon('play', "fa-play-circle fa-3x"),
                                                                  pauseButton = icon('pause', "fa-pause-circle fa-3x"))
                           )
             )
    )
    
  ),
  ## Add a footer for copyright
  tags$footer(HTML(copyRight), align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
)
  

server <- function(input, output, session) {
  # --------- Plots ---------- #
  Region <- reactive({
    filter(terror, region_txt == input$region)
  })
  
  observeEvent(Region(), {
    choices <- unique(Region()$country_txt)
    updateSelectInput(session, "country", choices = choices) 
  })
  
  output$distPlot <- renderPlot({
    terror.country=filter(terror, country_txt==input$country)
    col=paste(tolower(input$color), "1_txt", sep="")
    
    output$selected_var <- renderText({
      paste("Variables: Plotting", input$y,"vs Year, colored by",input$color)
      
    })
    
    output$tab <- renderUI({
      url <- a("Video Link", href="https://www.youtube.com/watch?v=R8nh2km174Q&feature=youtu.be")
      output$tab <- renderUI({
        tagList("Presentation:", url)
      })
    })
    
    output$Instruction <- renderText({
      paste("Choose a region first, then choose the country within the region")
      
    })
    
    
    if(input$y=="Success_rate"){
      ##Success Rate(2 variables)
      if(input$color=="Attacktype")
      {grouped <- terror.country %>% group_by(iyear,attacktype1_txt) %>%
        summarize(success_rate = sum(success) / n())}
      else if(input$color=="Weaptype")
      {grouped <- terror.country %>% group_by(iyear,weaptype1_txt) %>%
        summarize(success_rate = sum(success) / n())}
      else
      {grouped <- terror.country %>% group_by(iyear,targtype1_txt) %>%
        summarize(success_rate = sum(success) / n())}
      
      ggplot(data=grouped, aes(x = iyear, y = success_rate)) + geom_line(aes_string(color=col))+
        xlab("Year") +
        ylab("Success-Rate") +
        labs(color=input$color, caption="Data source: http://www.start.umd.edu/gtd/")  +
        theme(
          plot.title = element_text(color="red", size=18, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=18, face="bold"),
          axis.title.y = element_text(color="#993333", size=18, face="bold"),
          legend.title = element_text(colour="black", size = 16, face = "bold"),
          legend.text = element_text(colour="black", size = 16, face = "bold")
        )
    }
    
    else{
      ##Count
      #plot_ly(terror.country, type = 'scatter', mode = 'line', x = ~iyear, y=~nkill, color=~attacktype1_txt)
      
      ggplot(data=terror.country, aes(x = terror.country$iyear)) + geom_line(stat='count', aes_string(color=col))+
        xlab("Year") +
        ylab("Count") +
        labs(color=input$color, caption="Data source: http://www.start.umd.edu/gtd/")  +
        theme(
          plot.title = element_text(color="red", size=18, face="bold.italic"),
          axis.title.x = element_text(color="blue", size=18, face="bold"),
          axis.title.y = element_text(color="#993333", size=18, face="bold"),
          legend.title = element_text(colour="black", size = 16, face = "bold"),
          legend.text = element_text(colour="black", size = 16, face = "bold")
        )
      
    }
  })
  # --------- Pie chart ------- #
  output$distPie <- renderPlot({

    #pie(newdata$n, labels = newdata$Country1,col = rainbow(length(newdata$n)))
    data = terror[which(terror$iyear==input$yr),]

    tallies = data %>% count(attacktype1_txt)
    tallies$n = log10(tallies$n)
    pie(tallies$n,labels = tallies$attacktype1_txt, col = colors, 
        fill = "transparent")

  })

  #---------- Map ----------- #

  filteredGroup <- reactive({
    ## Get all terrorist cases for every country in one year
    grouped <- filteredTerrorData() %>% group_by(country_txt) %>% 
                summarize(terror_cases = n())
    ## Join the grouped terrorist cases dataset with world country. 
    ## There are some countries with
    ## missing terrorism data / no terrorism in that year at all,
    ## but they will be NAs in the joined dataset.
    CountryTerror <- left_join(data.frame(Name = WorldCountry$name), grouped, 
                               by = c("Name" ="country_txt"), ignore.case=TRUE) %>%
                               mutate(terror_cases = ifelse(
                                 is.na(terror_cases) & Name %in% obtainable, 0,  ## the country's data is obtainable, so we assume it's 0 case instead of NA
                                 terror_cases ## else just use whatever's in the field
                                ))
    print(head(CountryTerror, 20))
      #%>%
                      #mutate(terror_cases = ifelse(is.na(terror_cases), 0, terror_cases))
    return (CountryTerror)
  })
  
  filteredTerrorData <- reactive({
    filter(terror, iyear == input$yr)
  })

  output$map <- renderLeaflet({
    # Base map, which won't need to change dynamically with the slider input
    leaflet(WorldCountry) %>% addTiles() %>%
      fitBounds(min(terror$longitude), min(terror$latitude), 
                max(terror$longitude), max(terror$latitude))
  })

  # Incremental changes to the map. Whenever input year changes, erase the markers
  # and draw new ones without replacing the base map
  observe({
    req(input$tab_being_displayed == "Map and pie chart") # Only display if tab is 'Map Tab'
    filtered_group <- filteredGroup()  ## Get the filtered country terrorism cases data
    ## Create a log but turn all -Inf's into 0. 
    logged <- log(filtered_group$terror_cases)
    logged[logged == -Inf] <- 0
    
    pal <- colorBin(palette = "RdPu", domain = logged)
    leafletProxy("map", data = filtered_group) %>%
      clearShapes() %>% # remove the polygon
      clearControls() %>% # remove the legend
      ## Note that here addPolygons must specify that it uses WorldCountry,
      ## otherwise it would use the "filtered_group" for the shapes and then
      ## generate an error
      addPolygons(data = WorldCountry, 
                  fillColor = pal(logged),
                  fillOpacity = 0.5,
                  color = "grey",
                  highlight = highlightOptions(
                    weight = 3,
                    color = "red",
                    fillOpacity = 0.7,
                    bringToFront = FALSE)) %>%
      addLegend(pal = pal, values = logged,
                title = "Terror Cases",
                position = "bottomright",
                ## labFormat adjusts the number of bins -- and thus the "more", "less" labels
                ## dynamically,
                labFormat = function(type, cuts, p) { 
                  n = length(cuts) 
                  cuts[n] = "more" 
                  for (i in 2:(n-1)){cuts[i] = ""} 
                  cuts[1] = "less" 
                  paste0(cuts[-n], cuts[-1])}
      )
  })
  
  observe({
    req(input$tab_being_displayed == "Map and pie chart") # Only display if tab is 'Map Tab'
    
    ## If checkbox checked, display markers
    if (input$markers == TRUE){
      leafletProxy("map", data = filteredTerrorData()) %>%
        clearMarkers() %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude,
                         label = ~lapply(paste(attacktype1_txt, "<br/>", "Weapon: ",
                                               weaptype1_txt, "<br/>", country_txt, "<br/>", city), HTML),
                         color = ~color,
                         radius = 5,
                         fillOpacity = 0.7,
                         stroke = FALSE
        )
        
    } else {
      ## If checkbox not checked, don't display markers AND erase existing ones
      leafletProxy("map", data = filteredTerrorData()) %>%
        clearMarkers()
    }

  })
}

shinyApp(ui, server)