# load necessary packages
library(shiny)
library(leaflet)

library(dplyr)
library(plotly)
library(htmlwidgets)
library(DT)
library(lubridate)
library(shinyWidgets)
library(readr)
library(mapview)

tag<-read_rds("Data/tag.rds")
lf<-read_rds("Data/lf.rds")
rec<-read_rds("Data/rec.rds")

# DT table option
opt<-list( 
  dom = "Blfrtip"
  , buttons =  list(
    extend = "collection"
    , buttons = c("csv", "excel")
    , text = "Download"
  )  # end of buttons customization
  
  # customize the length menu
  , lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
  ) # end of lengthMenu customization
  , pageLength = 10
) # end of option

ui <- fluidPage(
  list(tags$head(
    HTML('<link rel="icon", href="Rplot.png",
                       type="image/png" />'),
    
    tags$style(
      HTML(
        "
      .navbar .navbar-nav {float: right;
                           color: #ff3368;
                           font-size: 18px;
                           background-color: #FFFF00 ; }
      .navbar .navbar-header {float: left; }
       .navbar-default .navbar-brand { color: blue;
                                       height: 55px;
                                       font-size: 28px;
                                      }

  "
      )
    )
  )),
  navbarPage(
    title =  "Cod Tagging",
    id = "Main",
    tabPanel(
      title = "Interactive Map",
      tags$head(
        includeCSS("styles2.css"),
        includeScript("google-analytics.js")
      ),
      # Include the customised CSS
      div(
        class = "outer",
        tags$style(
          type = "text/css",
          ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; padding: 0}"
        )
        ,
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = "100%"
        ),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          style = 'overflow-y:scroll;max-height:700px;',
          fixed = FALSE,
          draggable = FALSE,
          top = 15,
          left = "auto",
          right = 10,
          bottom = 5,
          width = 400,
          selectInput(
            inputId = "year",
            label = "Release Year",
            choices = c(unique(tag$Year)),
            selected = max(tag$Year)
          ),
          selectInput(
            inputId = "layer_choice",
            label = "Select map layer",
            choices = c("Recaptures", "Tagging Events"),
            selected = "Tagging Events"
          ),
          
          
          conditionalPanel(
            condition = "input.layer_choice == 'Recaptures'",
            selectInput(
              inputId = "x_variable",
              label = "Select X variable for plot",
              choices =
                c(
                  "Distance travelled",
                  "Days at Liberty",
                  "Growth (cm) at liberty",
                  "Length (cm) at Release"
                ),
              selected =
                "Days at Liberty"
            ),
            selectInput(
              inputId = "y_variable",
              label = "Select Y variable for plot",
              choices =
                c(
                  "Distance travelled",
                  "Days at Liberty",
                  "Growth (cm) at liberty",
                  "Length (cm) at Release"
                ),
              selected =
                "Distance travelled"
            ),
            selectInput(
              inputId = "color_variable",
              label = "Select parameter to color points by",
              choices =
                c(
                  "None",
                  "Distance travelled",
                  "Days at Liberty",
                  "Growth",
                  # "Gear",
                  "Sex",
                  "Maturity",
                  "Migratory Category",
                  "Event"
                ),
              selected =
                "None"
            ),
            actionButton("modal1", "Show plot")
          ),
          
          conditionalPanel(
            condition = "input.layer_choice == 'Tagging Events'",
            br(),
            htmlOutput("Tag"),
            htmlOutput("LF_title"),
            plotlyOutput("LF_hist")
          )
          ,
          br(),
          downloadButton(outputId = "dl", label = "Download Map")
          
        )
      )
    ),
    tabPanel(
      title = "Data Table",
      fluidRow(column(
        2,
        selectInput(
          inputId = "s1",
          label = "Select Data",
          choices =
            c("Recaptures", "Tagging Events"),
          selected =
            "Tagging Events"
        )
      ),
      column(
        10,
        conditionalPanel(condition = "input.s1 == 'Recaptures'",
                         fluidRow(
                           column(
                             3,
                             pickerInput(
                               inputId = "year2",
                               label = "Choose Release Year",
                               choices = sort(unique(rec$Year)),
                               selected =
                                 sort(unique(rec$Year)),
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                             )
                           ),
                           column(
                             3,
                             pickerInput(
                               inputId = "event",
                               label = "Choose Recapture Event",
                               choices = sort(unique(as.character(rec$Event))),
                               selected =
                                 unique(rec$Event),
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                             )
                             
                           )
                         ))
      )),
      
      fluidRow(DT::dataTableOutput("table"))
    ),
    tabPanel(
      title = "About",
      setBackgroundColor(
        color = c("#F7FBFF", "#2171B5"),
        gradient = "radial",
        direction = c("top", "left")
      ),
      htmlOutput("abouttext") ,
      htmlOutput("anglertextp1"),
      imageOutput("img1", width = "auto", height = 300),
      htmlOutput("img1txt"),
      br(),
      img(src = "Logos/split_logos.png", width = 550, style = "display: block;margin-top:2em")
    )
    
  )
) # end of ui

server <- function(input, output, session) {
  events <- reactive({
    filter(tag, Year == input$year)
  })
  forrec <- reactive({
    filter(rec, Year == input$year)
  })
  
  foundational.map <- reactive({
    if (input$layer_choice == "Recaptures") {
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = -3.5,
                lat = 53.8,
                zoom = 6)
      
    } else if (input$layer_choice == "Tagging Events") {
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = -3.5,
                lat = 53.8,
                zoom = 6)
      
      
      
    }
  }) # end of foundational.map()
  
  
  # render  leaflet map
  output$map <- leaflet::renderLeaflet({
    foundational.map()
    
  }) # end of render leaflet
  
  
  observe({
    if (input$layer_choice == "Recaptures") {
      recaps <- forrec()
      pal1 <-
        colorFactor("RdYlGn",
                    domain = recaps$distance_cat,
                    reverse = TRUE)
      leafletProxy('map') %>%
        clearMarkers() %>% clearControls() %>%
        addCircleMarkers(
          lng = recaps$Recap_Long_jit,
          lat = recaps$Recap_Lat_jit,
          fillOpacity = 0.8,
          color = pal1(recaps$distance_cat),
          stroke = FALSE,
          popup = paste(
            "<b>Long:</b> ",
            round(recaps$Recap_Long, 4),
            "<br />",
            "<b>Lat:</b> ",
            round(recaps$Recap_Lat, 4),
            "<br />",
            "<b>Release Date:</b>",
            recaps$Release_Date,
            "<br />",
            "<b>Recapture Date:</b>",
            recaps$Recap_Date,
            "<br />",
            "<b>Length at tagging:</b>",
            recaps$Release_Length_cm,
            "<br />",
            "<b>Length at recapture:</b>",
            recaps$Recap_Length_cm,
            "<br />",
            "<b>Growth:</b>",
            recaps$Growth,
            "<br />",
            "<b>Recapture Event:</b>",
            recaps$Event
          )
        ) %>%
        addLegend(
          "bottomleft",
          pal = pal1,
          values = recaps$distance_cat,
          title = "Migratory Category (from distance travelled)"
        ) %>%
        flyTo(lng = -3.5,
              lat = 53.8,
              zoom = 6)
    }
    else if (input$layer_choice == "Tagging Events") {
      tagging_events <- events()
      col <-
        colorNumeric("viridis", tagging_events$numbercodtagged, n = 5)
      leafletProxy('map') %>%
        clearMarkers() %>% clearControls() %>%
        addCircleMarkers(
          lng = tagging_events$Longitude,
          lat = tagging_events$Latitude,
          radius = tagging_events$numbercodtagged / 3,
          color = col(tagging_events$numbercodtagged),
          stroke = FALSE,
          fillOpacity = 0.9,
          popup = paste(
            "<b>Long:</b> ",
            round(tagging_events$Longitude, 4),
            "<br />",
            "<b>Lat</b>: ",
            round(tagging_events$Latitude, 4),
            "<br />",
            "<b>Year</b>",
            tagging_events$Year,
            "<br />",
            "<b>No. fish tagged</b>",
            tagging_events$numbercodtagged,
            "<br />",
            "<b>Survey</b>",
            tagging_events$Survey_ID,
            "<br />",
            "<b>Station</b>",
            tagging_events$Station
          )
        ) %>%
        flyTo(lng = -3.5,
              lat = 53.8,
              zoom = 7) %>%
        addLegend(
          "bottomleft",
          pal = col,
          values = tagging_events$numbercodtagged,
          title = "Number of Cod tagged"
        )
    }
  })
  
  # store the current user-created version
  # of the Leaflet map for download in
  # a reactive expression
  user.created.map <- reactive({
    # call the foundational Leaflet map
    
    if (input$layer_choice == "Recaptures") {
      recaps <- forrec()
      pal1 <-
        colorFactor("RdYlGn",
                    domain = recaps$distance_cat,
                    reverse = TRUE)
      foundational.map() %>%
        addCircleMarkers(
          lng = recaps$Recap_Long_jit,
          lat = recaps$Recap_Lat_jit,
          fillOpacity = 0.8,
          color = pal1(recaps$distance_cat),
          stroke = FALSE,
          popup = paste(
            "<b>Long:</b> ",
            round(recaps$Recap_Long, 4),
            "<br />",
            "<b>Lat:</b> ",
            round(recaps$Recap_Lat, 4),
            "<br />",
            "<b>Release Date:</b>",
            recaps$Release_Date,
            "<br />",
            "<b>Recapture Date:</b>",
            recaps$Recap_Date,
            "<br />",
            "<b>Length at tagging:</b>",
            recaps$Release_Length_cm,
            "<br />",
            "<b>Length at recapture:</b>",
            recaps$Recap_Length_cm,
            "<br />",
            "<b>Growth:</b>",
            recaps$Growth,
            "<br />",
            "<b>Recapture Event:</b>",
            recaps$Event
          )
        ) %>%
        addLegend(
          "bottomleft",
          pal = pal1,
          values = recaps$distance_cat,
          title = "Migratory Category (from distance travelled)"
        ) %>%
        # store the view based on UI
        setView(
          lng = input$map_center$lng
          ,
          lat = input$map_center$lat
          ,
          zoom = input$map_zoom
        )
    }
    else if (input$layer_choice == "Tagging Events") {
      tagging_events <- events()
      col <-
        colorNumeric("viridis", tagging_events$numbercodtagged, n = 5)
      foundational.map() %>%
        addCircleMarkers(
          lng = tagging_events$Longitude,
          lat = tagging_events$Latitude,
          radius = tagging_events$numbercodtagged / 3,
          color = col(tagging_events$numbercodtagged),
          stroke = FALSE,
          fillOpacity = 0.9,
          popup = paste(
            "<b>Long:</b> ",
            round(tagging_events$Longitude, 4),
            "<br />",
            "<b>Lat</b>: ",
            round(tagging_events$Latitude, 4),
            "<br />",
            "<b>Year</b>",
            tagging_events$Year,
            "<br />",
            "<b>No. fish tagged</b>",
            tagging_events$numbercodtagged,
            "<br />",
            "<b>Survey</b>",
            tagging_events$Survey_ID,
            "<br />",
            "<b>Station</b>",
            tagging_events$Station
          )
        ) %>%
        addLegend(
          "bottomleft",
          pal = col,
          values = tagging_events$numbercodtagged,
          title = "Number of Cod tagged"
        ) %>%
        # store the view based on UI
        setView(
          lng = input$map_center$lng
          ,
          lat = input$map_center$lat
          ,
          zoom = input$map_zoom
        )
    }
    
  })
  
  # end of creating user.created.map()
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$dl <- downloadHandler(
    filename = paste0(Sys.Date()
                      , "_customLeafletmap"
                      , ".pdf")
    
    ,
    content = function(file) {
      mapshot(
        x = user.created.map()
        ,
        file = file
        ,
        cliprect = "viewport"
        #the clipping rectangle matches the height & width from the viewing port
        ,
        selfcontained = FALSE
        #when this was not specified, the function for produced a PDF of two pages:
        #one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
  
  ###############################################
  ############## Gear of tagging vessel text ####
  ###############################################
  AnglerText <- read.csv('Data/tagging_program.csv', header = TRUE)
  #  output$anglertextp1 <- renderText(
  #   paste0(AnglerText[1, which(colnames(AnglerText)=="para1")])
  #  )
  # output$anglertextp2 <- renderText(
  #   paste0(AnglerText[1, which(colnames(AnglerText)=="para2")])
  # )
  # output$anglertextp3 <- renderText(
  #   paste0(AnglerText[1, which(colnames(AnglerText)=="para3")])
  # )
  output$img1 <- renderImage({
    img_file <- "www/tagging_kit.jpg"
    return(list(
      src = img_file,
      filetype = "image/jpg",
      width = 300
    ))
  }, deleteFile = FALSE)
  output$img1txt <- renderText(paste0(AnglerText[1, which(colnames(AnglerText) ==
                                                            "img1")]))
  output$maptext1 <- renderText(paste0(AnglerText[1, which(colnames(AnglerText) ==
                                                             "map1")]))
  
  output$abouttext <- renderText(paste0(AnglerText[1, which(colnames(AnglerText) ==
                                                              "about")]))
  
  ###############################################
  ############## Recapture plots ################
  ###############################################
  dataX <- reactive({
    recaps <- forrec()
    switch(
      input$x_variable,
      "Distance travelled" = recaps$Distance_nm,
      "Days at Liberty" = recaps$Days_at_Liberty,
      "Growth (cm) at liberty" = recaps$Growth,
      "Length (cm) at Release" = recaps$Release_Length_cm
    )
  })
  
  dataY <- reactive({
    recaps <- forrec()
    switch(
      input$y_variable,
      "Distance travelled" = recaps$Distance_nm,
      "Days at Liberty" = recaps$Days_at_Liberty,
      "Growth (cm) at liberty" = recaps$Growth,
      "Length (cm) at Release" = recaps$Release_Length_cm
    )
  })
  
  dataColor <- reactive({
    recaps <- forrec()
    switch(
      input$color_variable,
      "None" = "blue",
      #rep("1", len=dim(recaps)[1]),
      "Distance travelled" = recaps$Distance_nm,
      "Days at Liberty" = recaps$Days_at_Liberty,
      "Growth" = recaps$Growth,
      "Sex" = recaps$Sex,
      "Maturity" = recaps$Maturity,
      "Migratory Category" = recaps$distance_cat,
      "Event" = recaps$Event
    )
  })
  
  output$days_dist = renderPlotly({
    plot_ly(
      x = dataX(),
      y = dataY(),
      color = dataColor(),
      text =  ~ paste(
        input$x_variable,
        ":",
        round(dataX(), 0),
        "<br>",
        input$y_variable,
        ":",
        round(dataY(), 0)
      ),
      hoverinfo = 'text',
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        xaxis = list(title = input$x_variable),
        yaxis = list(title = input$y_variable)
      )
  })
  
  ###############################################
  ############## other plots ####################
  ###############################################
  
  output$Tag<-
    renderText(
      paste0(
        "<p><i><b> Total number of cod tagged in ",
        input$year," is ",sum(events()$numbercodtagged),
        " </b></i></p>"
      )
    )
  
  output$LF_title <-
    renderText(
      paste0(
        "<p><i><b>Figure 1: Length frequency distribution of tagged Cod in ",
        input$year,
        " </b></i></p>"
      )
    ) #<U></U>
  LF <- reactive({
    filter(lf, Year == input$year)
  })
  
  output$LF_hist = renderPlotly({
    plot_ly(
      x = LF()$Release_Length_cm,
      y = LF()$n,
      hoverinfo = 'text',
      text = ~ paste(
        '</br> Length: ',
        LF()$Release_Length_cm,
        '</br> Frequency: ',
        LF()$n
      ) ,
      type = 'bar'
    ) %>%
      layout(
        legend = list(title = list(text = "<b>Length</b>")),
        xaxis = list(title = "Length"),
        yaxis = list(title = "Frequency")
      )
  })
  output$table <- DT::renderDataTable(if (input$s1 == 'Recaptures') {
    L <-
      filter(rec,
             year(Release_Date) %in% input$year2 & Event %in% input$event)
    
    
    DT::datatable({
      L %>% select(
        'Release_Date',
        'Release_lat',
        'Release_long',
        'Recap_Date',
        'Recap_Lat',
        'Recap_Long',
        'Event',
        'Distance_nm',
        'Days_at_Liberty' ,
        'Growth'
      )
    }, extensions = 'Buttons'
    , options = opt) %>% formatRound(c(2, 3, 5, 6, 8), digits = 2)
  }
  else{
    DT::datatable({
      tag
    }, extensions = 'Buttons'
    , options = opt, filter = "top", rownames = F) %>% formatRound(c(4, 5), digits = 2)
  })
  ###############################################
  ############## Modal pop ups ##################
  ###############################################
  
  observeEvent(input$modal1, {
    showModal(modalDialog(
      plotlyOutput("days_dist"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
} # end of server

# run the Shiny app
shinyApp(ui = ui, server = server)

# end of script #
