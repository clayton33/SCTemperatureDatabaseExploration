library(shiny)
library(leaflet)
library(ggplot2)
debugInitializing <- FALSE # make FALSE when running app
if(debugInitializing){
  load('scTemperatureDatabase.rda')
  load('azmpProductPolygons.rda')
} else {
  load('../scTemperatureDatabase.rda') 
  load('../azmpProductPolygons.rda')
}

# get useful things out of database for the app
time <- as.POSIXct(as.numeric(df[['t_date']]), origin = '1970-01-01', tz = 'UTC')
years <- as.POSIXlt(time)$year + 1900
hasDepth <- ifelse(is.na(df[['depth_m']]),'no', 'yes')
df <- data.frame(df,
                 year = years,
                 time = time,
                 hasDepth = hasDepth)
uyear <- sort(unique(years))
uproject <- unique(df[['project']])
# when changing the year using just df, the app was slow, so do some 
#   pre-processing to make it faster, a simple split on the year should do it
dfs <- split(df, df[['year']])
# investigate max number of projects in a given year
nproj <- unlist(lapply(dfs, function(k) length(unique(k[['project']]))))
# add values to make initial tighter
lonlim <- range(df[['lon_dd']], na.rm = TRUE) + c(5, -5)
latlim <- range(df[['lat_dd']], na.rm = TRUE) + c(0, -18)

# define climatology ranges for wgf and sgf
climatologyRanges <- c(wgf = list(c('02-01', '04-30')),
                       sgf = list(c('06-01', '08-31'))
                       )

# define initial things for state
initialYear <- uyear[length(uyear)]
okyear <- which(as.numeric(names(dfs)) %in% initialYear)
dfsy <- split(dfs[[okyear]], dfs[[okyear]][['t_uid']])
initialLongitude <- unlist(lapply(dfsy, function(k) k[['lon_dd']][1]))
initialLatitude <- unlist(lapply(dfsy, function(k) k[['lat_dd']][1]))
initialProject <- unlist(lapply(dfsy, function(k) k[['project']][1])) # for map
initialTime <- unlist(lapply(dfsy, function(k) ifelse(length(k[['time']]) > 1,
                                                             paste(range(k[['time']]), collapse = ' to '),
                                                             paste(k[['time']])))) # for map
initialDepth <- unlist(lapply(dfsy, function(k) ifelse(length(k[['depth_m']]) > 1,
                                                              paste(range(k[['depth_m']]), collapse = ' to '),
                                                              paste(k[['depth_m']])))) # for map
initialClimatologyRanges <- lapply(climatologyRanges, function(k) as.POSIXct(paste(initialYear, k, sep = '-'), tz = 'UTC'))

# set groups for map
mapAllStations <- 'All stations'
mapReportingPolygons <- 'Reporting polygons'
# combine all polygons for plotting
plotPolygons <- c(wgf = list(data.frame(bndryWgf, name = 'Winter Groundfish Grid')), 
                  sgf = list(data.frame(bndrySgf, name = 'Summer Groundfish Grid')), 
                  areas = areaPolygons)
# function for being able to see project on axis
lineBreakString <- function(str, n) {gsub(paste0("([^ ]+( +[^ ]+){",n-1,"}) +"),
                              "\\1\n", str)}
# Define UI for application
ui <- fluidPage(
  titlePanel('Snow crab bottom temperature data'),
  fluidRow(
    column(2,
           wellPanel(
             selectInput(inputId = 'year',
                         label = '1. Choose a year to view',
                         choices = uyear,
                         selected = initialYear),
             actionButton("previousYear", shiny::HTML("&larr;")),
             actionButton("nextYear", shiny::HTML("&rarr;")),
             uiOutput(outputId = 'selectProject'),
             selectInput(inputId = 'histogramType',
                         label = '3. Choose count type for histogram',
                         choices = c('Total Observations' = 'observations',
                                     'Total Locations' = 'locations')),
             checkboxGroupInput(inputId = 'azmpProductAreas',
                                label = '4. Show relevant annual reporting product boundaries ?',
                                choices = c('Winter groundfish grid' = 'wgf',
                                            'Summer groundfish grid' = 'sgf',
                                            'Areas' = 'areas')),
             checkboxGroupInput(inputId = 'azmpProductClimatology',
                                label = '5. Show relevant annual reporting product climatology ranges ?',
                                choices = c('Winter groundfish grid' = 'wgf',
                                            'Summer groundfish grid' = 'sgf'))
           ) # closes wellPanel
           ), # closes column 2
    column(10,
           fluidRow(
            column(6,
                   leafletOutput(outputId = "map", height = 500)
            ), # closes column 12 for first fluidRow
            column(6,
                   plotOutput(outputId = 'samplingTiming', height = '500px'))
           ), # closes fluidRow
           fluidRow(
             column(12,
                    plotOutput(outputId = 'barplot')
                    ) # closes column 12 for second fluidRow
           ) # closes fluidRow
    ) # closes column 10 for first fluidRow
  ) # closes fluidRow
)

# Define server for application
server <- function(input, output) {
  # set state
  state <- reactiveValues(longitude = initialLongitude,
                          latitude = initialLatitude,
                          project = initialProject,
                          time = initialTime,
                          depth = initialDepth,
                          year = initialYear,
                          climatologyRanges = initialClimatologyRanges,
                          plotClimatologyRanges = NULL)
  # set uiOutputs
  ## selectProject based on the year chosen
  observeEvent(input$year, {
    projectsInYear <- c('All', unique(dfs[[which(as.numeric(names(dfs)) == input$year)]][['project']]))
    output$selectProject <- renderUI({
      selectInput(inputId = 'project',
                  label = '2. Choose a project to view',
                  choices = projectsInYear,
                  selected = projectsInYear[1])
    })
    # no project logic required since default is all for state variables
    # most variables used for map
    d <- dfs[[which(as.numeric(names(dfs)) %in% input$year)]]
    dd <- split(d, d[['t_uid']])
    state$longitude <- unlist(lapply(dd, function(k) k[['lon_dd']][1]))
    state$latitude <- unlist(lapply(dd, function(k) k[['lat_dd']][1]))
    state$project <- unlist(lapply(dd, function(k) k[['project']][1])) # for map
    state$time <- unlist(lapply(dd, function(k) ifelse(length(k[['time']]) > 1,
                                                                 paste(range(k[['time']]), collapse = ' to '),
                                                                 paste(k[['time']])))) # for map
    state$depth <- unlist(lapply(dd, function(k) ifelse(length(k[['depth_m']]) > 1,
                                                                  paste(range(k[['depth_m']]), collapse = ' to '),
                                                                  paste(k[['depth_m']])))) # for map
    # set state$climatologyRanges when input$year is changed
    state$climatologyRanges <- lapply(climatologyRanges, function(k) as.POSIXct(paste(input$year, k, sep = '-'), tz = 'UTC'))
    ok <- names(state$climatologyRanges) %in% input$azmpProductClimatology 
    if(all(!ok)){
      state$plotClimatologyRanges <- NULL
    } else {
      state$plotClimatologyRanges <- state$climatologyRanges[ok]
    }
    # update leaflet map with polygons when input$year is changed
    okPolygons <- unlist(lapply(input$azmpProductAreas, function(k) grep(k, names(plotPolygons)))) 
    polygonsToPlot <- plotPolygons[okPolygons]
    print(input$azmpProductAreas)
    # remove previous polygons
    leafletProxy('map') %>%
      clearGroup(group = mapReportingPolygons)
    # add to leaflet plot
    if(length(polygonsToPlot) != 0){
      for(i in 1:length(polygonsToPlot)){
        p <- polygonsToPlot[[i]]
        leafletProxy('map') %>%
          addPolygons(lng = p$longitude, lat = p$latitude,
                      color = 'darkgreen', fillOpacity = 0.2, stroke = TRUE, weight = 1, opacity = 0.5,
                      popup = paste(paste('Area name :', p[['name']][1]),
                                    sep = '<br/>'),
                      group = mapReportingPolygons,
                      options = pathOptions(pane = "polygons"))
      }
    }
  })
  
  # change state variables when certain things are selected
  observeEvent(input$year, {
    state$year <- input$year
  })
  observeEvent(input$histogramType, {
    state$histogramType <- input$histogramType
  })
  observeEvent(input$nextYear, {
    # get index of nextEvent
    okcurrent <- which(uyear == as.numeric(state$year))
    # handle boundary condition
    if(okcurrent == length(uyear)){
      ok <- okcurrent
    } else {
      ok <- okcurrent + 1 # do it by index instead of year     
    }
    # update select input for input$year
    updateSelectInput(inputId = 'year', selected = uyear[ok])
  })
  observeEvent(input$previousYear, {
    # get index of nextEvent
    okcurrent <- which(uyear == as.numeric(state$year))
    # handle boundary condition
    if(okcurrent == 1){
      ok <- okcurrent
    } else {
      ok <- okcurrent - 1 # do it by index instead of year     
    }
    # update select input for input$year
    updateSelectInput(inputId = 'year', selected = uyear[ok])
  })
  
  # when a project is changed, update state variables for map, and add polygons to map
  observeEvent(input$project, {
    d <- dfs[[which(as.numeric(names(dfs)) %in% input$year)]]
    dp <- split(d, d[['project']])
    if(input$project == 'All'){
      ok <- rep(TRUE, length = length(dp)) # use all data
    } else {
      ok <- names(dp) %in% input$project
    }
    dd <- do.call('c', lapply(dp[ok], function(k) split(k, k[['t_uid']])))
    state$longitude <- unlist(lapply(dd, function(k) k[['lon_dd']][1]))
    state$latitude <- unlist(lapply(dd, function(k) k[['lat_dd']][1]))
    state$project <- unlist(lapply(dd, function(k) k[['project']][1])) # for map
    state$time <- unlist(lapply(dd, function(k) ifelse(length(k[['time']]) > 1,
                                                              paste(range(k[['time']]), collapse = ' to '),
                                                              paste(k[['time']])))) # for map
    state$depth <- unlist(lapply(dd, function(k) ifelse(length(k[['depth_m']]) > 1,
                                                                 paste(range(k[['depth_m']]), collapse = ' to '),
                                                                 paste(k[['depth_m']])))) # for map
    okPolygons <- unlist(lapply(input$azmpProductAreas, function(k) grep(k, names(plotPolygons)))) 
    polygonsToPlot <- plotPolygons[okPolygons]
    print(input$azmpProductAreas)
    # add to leaflet plot when project is changed
    if(length(polygonsToPlot) != 0){
      for(i in 1:length(polygonsToPlot)){
        p <- polygonsToPlot[[i]]
        leafletProxy('map') %>%
          addPolygons(lng = p$longitude, lat = p$latitude,
                      color = 'darkgreen', fillOpacity = 0.2, stroke = TRUE, weight = 1, opacity = 0.5,
                      popup = paste(paste('Area name :', p[['name']][1]),
                                    sep = '<br/>'),
                      group = mapReportingPolygons,
                      options = pathOptions(pane = "polygons"))
      }
    }
  })
  observeEvent(input$azmpProductClimatology, ignoreNULL = FALSE, {
    ok <- names(state$climatologyRanges) %in% input$azmpProductClimatology 
    if(all(!ok)){
      state$plotClimatologyRanges <- NULL
    } else {
      state$plotClimatologyRanges <- state$climatologyRanges[ok]
    }
  })
  # samplingTiming plot
  output$samplingTiming <- renderPlot({
    okyear <- which(as.numeric(names(dfs)) %in% input$year)
    d <- dfs[[okyear]]
    upd <- unique(d[['project']])
    updydf <- data.frame(project = upd,
                         idx = 1:length(upd))
    idx <- updydf$idx[match(d[['project']], updydf[['project']])]
    d <- data.frame(d, 
                    idx = idx)
    par(mar = c(3, 7, 0.25, 0.25))
    xlim <- as.POSIXct(paste(input$year, c('01-01', '12-31'), sep = '-'), tz = 'UTC')    
    plot(d[['time']], d[['idx']],
         col = 'white', # going to redraw after putting grid on
         xlab = '', ylab = '',
         yaxt = 'n',
         xlim = xlim)
    axis(2, at = updydf[['idx']], labels = lineBreakString(updydf[['project']],1), las = 1, cex.axis = 4/5)
    # add grid
    abline(h = updydf[['idx']], lty = 'dotted', col = 'lightgrey')
    abline(v = seq(xlim[1], xlim[2], by = 'month'), lty = 'dotted', col = 'lightgrey')
    points(d[['time']], d[['idx']])
    if(!is.null(state$plotClimatologyRanges)){
      lapply(state$plotClimatologyRanges, function(k) abline(v = k, lty = 'dashed', col = 'darkgrey'))
    }
  })
  # barplot plot
  output$barplot <- renderPlot({
    okyear <- which(as.numeric(names(dfs)) %in% input$year)
    d <- dfs[[okyear]]
    if(state$histogramType == 'observations'){
      dat <- data.frame(table(d[['project']], d[['hasDepth']]))
      names(dat) <- c("project","hasDepth","Count")      
    }
    if(state$histogramType == 'locations'){
      ds <- split(d, d[['t_uid']])
      dat <- data.frame(table(unlist(lapply(ds, function(k) k[['project']][1])),
                              unlist(lapply(ds, function(k) k[['hasDepth']][1]))))
      names(dat) <- c("project","hasDepth","Count")      
    }
    ggplot(data=dat, aes(x=project, y=Count, fill=hasDepth)) + geom_bar(stat="identity")
  })
  # leaflet plot
  output$map <- renderLeaflet({
    leaflet() %>%
    addMapPane("points", zIndex = 420) %>% # for controlling order, larger value = more on top
    addMapPane("polygons", zIndex = 410) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    fitBounds(lng1 = lonlim[1],
              lat1 = latlim[1],
              lng2 = lonlim[2],
              lat2 = latlim[2]) %>%
    # use NOAA graticules
    # not sure if it does much, but it allows to zoom further in
    # no bathy when zoomed less than 500m though.
    addWMSTiles(
      "https://maps.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
      layers = c("1-degree grid", "5-degree grid"),
      options = WMSTileOptions(format = "image/png8", transparent = TRUE),
      attribution = "NOAA") %>%
    # add extra map features
    addScaleBar(position = 'topright')%>%
    addMeasure(primaryLengthUnit = "kilometers",
               secondaryLengthUnit = 'miles',
               primaryAreaUnit = "hectares",
               secondaryAreaUnit="acres",
               position = 'bottomleft') %>%
    addCircleMarkers(lng = state$longitude,
                     lat = state$latitude,
                     #layerId = eventNumber,
                     radius = 4, fillOpacity = 0.5, stroke = F,
                     popup = paste(paste('Project :', state$project),
                                   paste('Time :', state$time),
                                   paste('Depth:', state$depth),
                                   sep = '<br/>'),
                     group = mapAllStations,
                     options = pathOptions(pane = "points")
                     ) %>%
    addLayersControl(overlayGroups = c(mapAllStations,
                                       mapReportingPolygons))
  }) # closes leafletplot
  observeEvent(input$azmpProductAreas, ignoreNULL = FALSE, {
    okPolygons <- unlist(lapply(input$azmpProductAreas, function(k) grep(k, names(plotPolygons)))) 
    polygonsToPlot <- plotPolygons[okPolygons]
    print(input$azmpProductAreas)
    # remove previous polygons
    leafletProxy('map') %>%
      clearGroup(group = mapReportingPolygons)
    # add to leaflet plot
    if(length(polygonsToPlot) != 0){
      for(i in 1:length(polygonsToPlot)){
        p <- polygonsToPlot[[i]]
        leafletProxy('map') %>%
        addPolygons(lng = p$longitude, lat = p$latitude,
                         color = 'darkgreen', fillOpacity = 0.2, stroke = TRUE, weight = 1, opacity = 0.5,
                         popup = paste(paste('Area name :', p[['name']][1]),
                                       sep = '<br/>'),
                         group = mapReportingPolygons,
                    options = pathOptions(pane = "polygons"))
      }
    }
  }) # closes observeEvent for input plot event to add to leaflet
}

# Run the application 
shinyApp(ui = ui, server = server)
