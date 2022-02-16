library(shiny)
library(sf)
library(leaflet)
library(arrow)
library(aws.s3)
library(dplyr)

Sys.getenv()

ui <-
    bootstrapPage(
        tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
        leafletOutput("map", width = "100%", height = "100%"),
        # position and properties of the time slider
        absolutePanel(
            top = 10,
            left = 100,
            draggable = TRUE,
            # slider title, step increments, and ticks
            shiny::selectInput("path", "Gage ID",
                               choices = NULL),
            shiny::selectInput("att", "Attribute",
                               choices = NULL)
        )
    )



# Define server logic
server <- function(input, output, session) {
    bucket = 'formulations-dev'
    version = "CAMELS20"
    xx = as.vector(sapply(get_bucket(
        bucket = bucket, prefix = version
    ), "[[", 1))
    xx = xx[!grepl("spatial|parameters|reference", xx)]
    dirs = paste0('s3://', bucket, '/', xx[grep("camels", xx)])
    names(dirs) = sapply(strsplit(basename(dirs), "_"), "[[", 2)

    shiny::updateSelectInput(session, "path", choices = dirs, selected = "")

    observeEvent(input$path, {
        if (input$path != "") {
            p = paste0(input$path, "spatial/hydrofabric.gpkg")
            atts = paste0(input$path, "parameters/attributes.parquet")

            c = arrow::open_dataset(atts)$schema$names
            shiny::updateSelectInput(session,
                                     "att",
                                     choices = c[-1],
                                     selected = "")

            cats <<-
                st_transform(s3read_using(read_sf, object = p, "catchments"),
                             4326)
            fps  <<-
                st_transform(s3read_using(read_sf, object = p,   "flowpaths"),
                             4326)
            nex  <<-
                st_transform(s3read_using(read_sf, object = p,   "nexus"), 4326)
            bb = sf::st_bbox(cats)

            leaflet::leafletProxy("map") |>
                clearShapes() |>
                addPolygons(data = cats,
                            fillColor = 'gray',
                            color = "black") |>
                addPolylines(data = fps,
                             weight = 1,
                             opacity = 1) |>
                addCircles(
                    data = nex,
                    radius = 100,
                    fillOpacity = 1,
                    fillColor = "red",
                    stroke = FALSE
                ) |>
                fitBounds(
                    lat1 = as.numeric(bb$ymin),
                    lat2 = as.numeric(bb$ymax),
                    lng1 = as.numeric(bb$xmin),
                    lng2 = as.numeric(bb$xmax)
                )
        }
    })

    observeEvent(input$att, {
        if (input$att != "") {
            atts = paste0(input$path, "parameters/attributes.parquet")

            opd = arrow::open_dataset(atts)

            selection = input$att
            print(selection)

            out =  left_join(cats,
                             collect(select(opd, "ID", !!selection)),
                             by = "ID")

            v = out[[selection]]
            qpal <- colorNumeric("RdYlBu", v)

            leaflet::leafletProxy("map") |>
                clearShapes() |>
                clearControls() |>
                addPolygons(
                    data = out,
                    stroke = TRUE,
                    smoothFactor = 0.2,
                    color = "black",
                    fillOpacity = 1,
                    opacity = 1,
                    fillColor = ~ qpal(v),
                    label = v
                ) %>%
                addPolylines(data = fps,
                             weight = 1,
                             opacity = 1) |>
                addCircles(
                    data = nex,
                    radius = 100,
                    fillOpacity = 1,
                    fillColor = "red",
                    stroke = FALSE
                ) |>
                addLegend(
                    position = "bottomleft",
                    pal = qpal,
                    values = v,
                    opacity = 1,
                    title = selection
                )
        }
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
