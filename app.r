###################################################
# Tiger Tracks
# robert franssen
###################################################

###################################################
# Load libraries
###################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(htmlwidgets)
library(DT)
library(leaflet)
library(lubridate)
library(data.table)
library(sp)
library(sqldf)
library(rgdal)
library(leaflet)
library(dplyr)
library(adehabitatHR)
library(shinyalert)
require(rgeos)


###################################################
# Prep and load Tiger Tracking Data
###################################################

detections_dt <- fread('Tigers/Tigers.csv')
detections_dt$BeginTime <- as.Date(detections_dt$BeginTime, "%d/%m/%y")

# add unique identifier
detections_dt$id <- seq.int(nrow(detections_dt))

# factorize tigername # TODO: fix downstream representation in datatable
#detections_dt$TigerName <- as.factor(detections_dt$TigerName)

#############################################################
# Create Map Labels and Color Palettes
#############################################################

circle_labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  detections_dt$TigerName,
  detections_dt$BeginTime
) %>% lapply(htmltools::HTML)

detections_pal <-  leaflet::colorFactor(palette="viridis", 
                                        domain = unique(detections_dt$TigerName), 
                                        na.color="transparent")

###################################################
# Alternative Logo (from dashboardthemes)
###################################################
# custom_header <- shinyDashboardLogoDIY(
#   #boldText=paste(icon('globe'), 'Tiger')
#   boldText='Tiger'
#   , mainText="| Tracks"
#   , textSize=16
#   , badgeText="DEV"
#   , badgeTextColor="white"
#   , badgeTextSize=2
#   , badgeBackColor="#40E0D0"
#   , badgeBorderRadius=3
# )

###################################################
# UI Function
###################################################

ui <- navbarPage(
  
  ###################################################
  # header
  ###################################################
  #title=custom_header,
  title="Tiger | Tracks",
  theme = shinythemes::shinytheme("flatly"),
  
  ###################################################
  # ui/map/start
  ###################################################
  tabPanel(
    title="Tiger Map",
    useShinyalert(),  # Set up shinyalert
    sidebarLayout(
      sidebarPanel(
        # Make the sliderInput play button a little bigger
        tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
        sliderInput(inputId="detection_dates",
                    label="Start and End Dates",
                    min=min(detections_dt$BeginTime),
                    max=max(detections_dt$BeginTime),
                    value=c(min(detections_dt$BeginTime), max(detections_dt$BeginTime)),
                    animate=TRUE,
                    dragRange=TRUE),
        pickerInput(
          inputId = "tiger_picker",
          label = "Choose a Tiger",
          choices = unique(detections_dt$TigerName),
          multiple=TRUE,
          selected = unique(detections_dt$TigerName),
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = "[Deselect All]",
            `select-all-text` = "All the tigers!",
            size = 10,
            `selected-text-format` = "count > 4",
            `count-selected-text` = "{0} tigers selected"
          )
        ),
        checkboxInput(
          inputId = "show_paths",
          label = "Show Paths",
          value=FALSE
        ),
        radioButtons(
          inputId = "model_select",
          label = "Home Range Overlay",
          choices = c('None', 'Kernel Density', 'Convex Hull'),
          selected = 'None'
        ),
        knobInput(
          inputId = "prob_knob",
          label = "Confidence",
          value = 90,
          min = 50,
          max=99,
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#428BCA",
          inputColor = "#428BCA"
        )
      ),
      mainPanel(
        #tags$style(type = "text/css", "#tiger_map {height: calc(100vh - 80px) !important;}"), # use height in leafletOutput instead
        box(width=12,
            height=800,
            # box title
            title = "Tigers in Panna National Park", 
            #title=textOutput("dygraph_box_title_js_2"),
            status = "success",
            leafletOutput("tiger_map", height = "75vh"),
            footer=tags$a(href="https://www.zoatrack.org/", "(2019) Data from: ZoaTrack.org Date Accessed: 01 Sep 2020"),
            collapsible=FALSE
        )
      )
    )
  ),
  
  ###################################################
  # ui/map/end
  ###################################################
  
  ###################################################
  # ui/more/start
  ###################################################
  
  # navbarMenu("More",
  tabPanel("Data",
           fluidRow(
             column(12,
                    includeMarkdown("data_hdr.md")
             )
           ),
           fluidRow(
             column(12,
                    DT::dataTableOutput("table")
             )
           )
  ),
  tabPanel("Code",
           fluidRow(
             column(12,
                    includeMarkdown("code.md")
             )
           )
  ),
  tabPanel("User Guide",
           fluidRow(
             column(12,
                    includeMarkdown("about.md")
             )
           )
  )
  # )
  
  ###################################################
  # ui/more/end
  ###################################################
)


###################################################
# Server function
###################################################
server <- function(input, output, session) {
  
  ###################################################
  # server/tiger-map/start
  ###################################################
  
  # Setting static bounds so the 'playing' the time slider holds the same frame of reference even after i start subsetting detections_dt
  map_bounds <- data.frame(lng1 = min(detections_dt$X_DecimalDegrees),
                           lng2 = max(detections_dt$X_DecimalDegrees),
                           lat1 = min(detections_dt$Y_DecimalDegrees),
                           lat2 = max(detections_dt$Y_DecimalDegrees))
  
  output$tiger_map <- renderLeaflet({
    
    tiger_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      
      fitBounds(lng1=map_bounds$lng1, 
                lat1=map_bounds$lat1, 
                lng2=map_bounds$lng2, 
                lat2=map_bounds$lat2) %>%
      
      # Base groups
      addProviderTiles(providers$Esri.WorldStreetMap, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE,
        group = "Esri.WorldStreetMap"
      )) %>%
      
      #addCircles(lat=~latitude,
      addCircleMarkers(data=detections_dt,
                       lat=~Y_DecimalDegrees,
                       lng=~X_DecimalDegrees,
                       #radius=5000, # km
                       radius=5, # pixels
                       color = ~detections_pal(TigerName),
                       stroke = FALSE, 
                       fillOpacity = 0.5,
                       fillColor=~detections_pal(TigerName),
                       weight = 2,
                       opacity = 1,
                       dashArray = "3",
                       #label = ~TigerName,
                       label=circle_labels,
                       #layerId = ~id,
                       group = "Detections",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto"))%>%
      
      # Layers control
      addLayersControl(baseGroups = c("Esri.WorldStreetMap"),
                       overlayGroups = c("Detections"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      
      #hideGroup("Paths") %>% # Made this a user input instead and put it in leafletProxy
      
      addLegend(pal = detections_pal,
                values = unique(detections_dt$TigerName),
                opacity = 0.7,
                title = 'Tigers',
                position = "bottomright")
    
    # # Adding basemaps/provider tiles here #TODO: not rendering properly
    # providers <- c("Stamen.TonerLite", "Stamen.Watercolor", "CartoDB.Positron", "Acetate.terrain")
    # 
    # 
    # for(i in 1:length(providers)){
    #   tiger_map <- tiger_map %>% addProviderTiles(providers[i], group = providers[i])
    # }
    # 
    # tiger_map <- tiger_map %>% addLayersControl(
    #   baseGroups = providers,
    #   overlayGroups = c("Detections"),
    #   options = layersControlOptions(collapsed = FALSE))
    
    return(tiger_map)
  })
  
  # Listen for multiple events (tiger picker and date picker)
  toListen <- reactive({
    list(input$tiger_picker, input$detection_dates, input$show_paths, input$model_select, input$prob_knob)
  })
  
  observeEvent(toListen(), {
    
    selected_tigers <- input$tiger_picker
    
    # Subset the data based on the input
    
    # the datafame
    detections_dt <- subset(detections_dt, TigerName %in% selected_tigers)
    detections_dt <- subset(detections_dt, BeginTime >= input$detection_dates[1] & BeginTime <= input$detection_dates[2])
    
    # If the tiger selection has been updated, update the labels else clear them out
    if (length(selected_tigers)>0){
      # update labels
      circle_labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        detections_dt$TigerName,
        detections_dt$BeginTime
      ) %>% lapply(htmltools::HTML)
    } else{
      circle_labels = ''
    }
    
    leafletProxy("tiger_map") %>%
      
      # Detections
      # Clear groups
      clearGroup(group="Detections") %>%
      clearGroup(group="Paths") %>%
      clearGroup(group="Models") %>%
      clearControls() %>%
      
      #addCircles(lat=~latitude,
      addCircleMarkers(data=detections_dt,
                       lat=~Y_DecimalDegrees,
                       lng=~X_DecimalDegrees,
                       #radius=5000, # km
                       radius=5, # pixels
                       color = ~detections_pal(TigerName),
                       stroke = FALSE, 
                       fillOpacity = 0.5,
                       fillColor=~detections_pal(TigerName),
                       weight = 2,
                       opacity = 1,
                       dashArray = "3",
                       label=circle_labels,
                       #layerId = ~id,
                       group = "Detections",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")) %>%
      
      addLegend(pal = detections_pal,
                values = unique(detections_dt$TigerName),
                opacity = 0.7,
                title = 'Tigers',
                position = "bottomright")
    
    # Add Polylines/Paths based on user input
    if (input$show_paths == TRUE) {
      
      for (i in selected_tigers) {
        leafletProxy("tiger_map") %>%
          addPolylines(
            data = detections_dt[detections_dt$TigerName == i,],
            lng = ~ X_DecimalDegrees,
            lat = ~ Y_DecimalDegrees,
            opacity = 0.1,
            group = "Paths",
            color = ~ detections_pal(TigerName)
          )
        # TODO: these just reallllly slow things down - can i speed up?
        # label=circle_labels,
        # labelOptions = labelOptions(
        #   style = list("font-weight" = "normal", padding = "3px 8px"),
        #   textsize = "15px",
        #   direction = "auto"))
      }
    } else{
      leafletProxy("tiger_map") %>%
        clearGroup("Paths")
    }
    
    # Add Models
    
    # Wrapping this in a try() because there are still combinations of tigers and dates that are throwing extent errors
    # but we can silently ignore them because they are not practical use cases. Best to keep the app running and allow the
    # viewer to change one of the inputs
    try(
      
      # this works but id rather ignore it
      if (input$model_select == 'Kernel Density' & length(selected_tigers) >= 1) {
        
        # this shinyalert works but id rather ignore it
        # Get detection counts
        # min_detection_count <- sqldf("select min(detections) as min_detection_count from (select tigername, count(*) as detections from detections_dt group by 1) as x;")
        # #min_detection_count <- coalesce(min_detection_count, 0)
        # 
        # if (min_detection_count < 5) {
        #   shinyalert("Expand date slider", "Estimation requires additional detections", type = "info")
        #   #break
        #   
        # } else {        # min_detection_count <- sqldf("select min(detections) as min_detection_count from (select tigername, count(*) as detections from detections_dt group by 1) as x;")
        # #min_detection_count <- coalesce(min_detection_count, 0)
        # 
        # if (min_detection_count < 5) {
        #   shinyalert("Expand date slider", "Estimation requires additional detections", type = "info")
        #   #break
        #   
        # } else {
        detections_sp <- SpatialPointsDataFrame(coords = detections_dt[, c('X_DecimalDegrees', 'Y_DecimalDegrees')], detections_dt)
        
        tigers_kde <- kernelUD(detections_sp[, 2]) # make sure this is indexed to the grouping variable
        tigers_kde_polys <- getverticeshr(tigers_kde, percent = input$prob_knob)
        
        leafletProxy("tiger_map") %>%
          addPolygons(
            data = tigers_kde_polys,
            color = ~ detections_pal(id),
            fillOpacity = 0.5,
            fillColor =  ~ detections_pal(id),
            group = "Models"
          )
        
        #} # end of shinyAlert logic that i turned off
        
      } else if (input$model_select == 'Convex Hull' & length(selected_tigers) >= 1) {
        
        detections_sp <- SpatialPointsDataFrame(coords = detections_dt[, c('X_DecimalDegrees', 'Y_DecimalDegrees')], detections_dt)
        tigers_mcp_polys <- mcp(detections_sp[, 2], percent = input$prob_knob)
        
        for (i in selected_tigers) {
          leafletProxy("tiger_map") %>%
            addPolygons(
              data = tigers_mcp_polys,
              color = ~ detections_pal(id),
              fillOpacity = 0.1,
              fillColor =  ~ detections_pal(id),
              group = "Models"
            )
        }
        
      }
      
      else {
        leafletProxy("tiger_map") %>%
          clearGroup("Models")
      }
      
      ,  silent=TRUE) # ending the try function
    
    
  })
  
  ###################################################
  # server/example/end
  ###################################################
  
  ###################################################
  # server/about/start
  ###################################################
  
  output$table <- DT::renderDataTable({
    DT::datatable(detections_dt[1:25,],
                  extensions='Buttons',
                  caption="",
                  filter="top",
                  rownames=FALSE,
                  #width=400,
                  #scrollX=TRUE,
                  options=list(
                    dom='Bfrtip',
                    pageLength=10,
                    autoWidth=TRUE,
                    #colnames=c(''),
                    buttons=list('copy', 'print', list(
                      extend='collection',
                      buttons=c('csv', 'excel', 'pdf'),
                      text='Download'
                    )
                    )
                  ))
  }, server=FALSE)
  
  ###################################################
  # server/about/end
  ###################################################
  
}
###################################################
# Call function
###################################################
shinyApp(ui, server)
#shinyApp(ui, server, options = list(display.mode = 'showcase'))
