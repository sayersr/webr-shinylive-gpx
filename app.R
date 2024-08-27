library(shiny)
library(leaflet)
library(lubridate)

ui <- fluidPage(
  titlePanel("GPX Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("gpx_file", "Upload GPX File", accept = ".gpx"),
      numericInput("time_adjust", "Adjust Time (hours):", 0, min = -23, max = 23, step = 1),
      sliderInput("time_select", "Select Time:", min = as.POSIXct("2000-01-01"), max = as.POSIXct("2000-01-02"), value = as.POSIXct("2000-01-01")),
      textInput("precise_time", "Precise Time (YYYY-MM-DD HH:MM:SS):"),
      actionButton("play_button", "Play/Pause")
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("current_time"),
      verbatimTextOutput("debug_output")
    )
  )
)

server <- function(input, output, session) {
  gpx_data <- reactiveVal(NULL)
  adjusted_times <- reactiveVal(NULL)

  # Initialize an empty map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
  })

  observeEvent(input$gpx_file, {
    req(input$gpx_file)
    tryCatch({
      gpx_xml <- read_xml(input$gpx_file$datapath)
      ns <- xml_ns(gpx_xml)

      gpx_time <- tryCatch({
        as.POSIXct(xml_text(xml_find_all(gpx_xml, "//d1:trkpt/d1:time", ns)), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      }, error = function(e) {
        as.POSIXct(xml_text(xml_find_all(gpx_xml, "//trkpt/time")), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      })

      lat <- as.numeric(xml_attr(xml_find_all(gpx_xml, "//d1:trkpt", ns), "lat"))
      lon <- as.numeric(xml_attr(xml_find_all(gpx_xml, "//d1:trkpt", ns), "lon"))

      gpx <- st_as_sf(data.frame(time = gpx_time, lon = lon, lat = lat), 
                      coords = c("lon", "lat"), crs = 4326)
      gpx_data(gpx)
      adjusted_times(gpx_time)

      coords <- st_coordinates(gpx)
      
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolylines(lng = coords[, "X"], lat = coords[, "Y"], color = "blue", weight = 3) %>%
        fitBounds(
          lng1 = min(coords[, "X"]), lat1 = min(coords[, "Y"]),
          lng2 = max(coords[, "X"]), lat2 = max(coords[, "Y"])
        )

      print("GPX data loaded and map updated")
    }, error = function(e) {
      print(paste("Error processing GPX file:", e$message))
    })
  })

  observe({
    req(gpx_data(), input$time_adjust)
    gpx <- gpx_data()
    adjusted_times(gpx$time + hours(input$time_adjust))
  })

  output$time_slider <- renderUI({
    req(adjusted_times())
    times <- adjusted_times()
    if (length(times) == 0) {
      return(NULL)
    }
    sliderInput(
      "time_select",
      "Select Time:",
      min = min(times),
      max = max(times),
      value = min(times),
      step = 1,
      animate = animationOptions(interval = 500, loop = TRUE)
    )
  })

  observe({
    req(input$time_select, gpx_data(), adjusted_times())
    gpx <- gpx_data()
    adj_times <- adjusted_times()
    selected_time <- input$time_select

    # Find the index of the closest adjusted time
    closest_index <- which.min(abs(difftime(adj_times, selected_time)))
    current_point <- gpx[closest_index,]
    
    print(paste("Selected time:", format(selected_time, "%Y-%m-%d %H:%M:%S")))
    print(paste("Closest point time:", format(current_point$time, "%Y-%m-%d %H:%M:%S")))
    print(paste("Adjusted time:", format(adj_times[closest_index], "%Y-%m-%d %H:%M:%S")))
    print(paste("Marker coordinates:", st_coordinates(current_point)[1], st_coordinates(current_point)[2]))

    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = current_point, popup = format(adj_times[closest_index], "%Y-%m-%d %H:%M:%S"))
  })

  output$current_time <- renderUI({
    req(input$time_select)
    wellPanel(
      h4("Current Time"),
      p(format(input$time_select, "%Y-%m-%d %H:%M:%S"))
    )
  })

  output$debug_output <- renderPrint({
    if (!is.null(gpx_data())) {
      gpx <- gpx_data()
      adj_times <- adjusted_times()
      cat("GPX Data Summary:\n")
      print(summary(gpx))
      cat("\nNumber of points:", nrow(gpx), "\n")
      cat("\nBounding Box:\n")
      print(st_bbox(gpx))
      cat("\nOriginal Time range:\n")
      print(range(gpx$time))
      cat("\nAdjusted Time range:\n")
      print(range(adj_times))
      cat("\nTime Adjustment (hours):\n")
      print(input$time_adjust)
      if (!is.null(input$time_select)) {
        cat("\nCurrent selected time:\n")
        print(input$time_select)
      }
    } else {
      cat("No GPX data loaded yet.")
    }
  })
}

shinyApp(ui = ui, server = server)
