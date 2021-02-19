library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(scales)

source("utils.R")
## Set initial topic choices
choices_topics <- sort(unique(demographic_data_long$topic))

##### UI after 5/17/2020 16:39 #####
ui <- fluidPage(
    theme = shinytheme("flatly"), ##(lumen, paper, simplex, flatly, yeti) ## See interactive themes: https://gallery.shinyapps.io/117-shinythemes/
    titlePanel("Demographic Shaper"),
    
    sidebarPanel(
        selectInput(inputId = "topic", label = "Select Topic", choices = choices_topics),
        selectInput(inputId = "variable", label = "Select Variable", choices = NULL),
        selectInput(inputId = "subgroup", label = "Select Subgroup or Race", choices = NULL, selected = "All"),
        radioButtons(inputId = "parameter", label = "Select Percent or Total", choices = c("Percent (of Subgroup in Universe)" = "Percent", "Total, Count or Median" = "Total"), selected = "Total"),
        sliderInput(inputId = "year", label = "Select Year", min = 2010, max = 2019, value = 2019, sep = "")
    ),
    mainPanel(
        leafletOutput("mymap"),
        textOutput("mytext"),
        plotOutput("myplot")
    )
             
)


server <- function(input, output, session) {
    
    
    ######### Demographic Mapping ##########
    choices_variables <- reactive({
        choices_variables <- sort(unique(demographic_data_long$variable[which(demographic_data_long$topic == input$topic & demographic_data_long$parameter == input$parameter)]))
    })
    
    choices_subgroups <- reactive({
        choices_subgroups <- sort(unique(demographic_data_long$subgroup[which(demographic_data_long$topic == input$topic & demographic_data_long$variable == input$variable )]))
    })
    
    observe({
        updateSelectInput(session = session, inputId = "variable", choices = choices_variables())
    })
    
    observe({
        updateSelectInput(session = session, inputId = "subgroup", choices = choices_subgroups())
    })
    
    input_filtered_data <- reactive({
        
        data <- demographic_data_long %>%
            filter(topic == input$topic,
                   variable == input$variable,
                   subgroup == input$subgroup,
                   parameter == input$parameter,
                   year == input$year) %>%
            left_join(., msa.sf, by = "GEOID") %>% st_as_sf()

        validate(
            need(nrow(data) >= 2, "WARNING: The margins of error for this query are very high. Proceed with extreme caution.\nTo continue, please select 'Yes - show unreliable estimates'.\n\nThis error might occur for a second reason: Some topics are not available for all years. They include:
           Internet Access (2017 through 2019 only)
           Health Insurance (2013 through 2019 only)
           Disability (2012 through 2019 only)")
        )
        return(data)
      # browser()
    })
    
    input_filtered_data_ts <- reactive({
        
        data <- demographic_data_long %>%
            filter(topic == input$topic,
                   variable == input$variable,
                   subgroup == input$subgroup,
                   parameter == input$parameter)

        validate(
            need(nrow(data) >= 2, "WARNING: The margins of error for this query are very high. Proceed with extreme caution.\nTo continue, please select 'Yes - show unreliable estimates'.\n\nThis error might occur for a second reason: Some topics are not available for all years. They include:
           Internet Access (2017 through 2019 only)
           Health Insurance (2013 through 2019 only)
           Disability (2012 through 2019 only)")
        )
        
        return(data)
    })
    
    output$myplot <- renderPlot({
        
        summary <- input_filtered_data_ts() %>% 
            summarize_tracts(tract_ids = selected_GEOIDs$GEOIDs)
        
        uni <- summary$universe
        var <- summary$variable
        sub <- summary$subgroup
        est <- summary$est
        moe <- summary$moe
        
        summary %>%
            ggplot(aes(x = year, y = est)) +
            geom_line(size = 2) +
            geom_ribbon(aes(ymax= est+moe, ymin=est-moe), 
                        alpha=0.2) +
            scale_x_continuous(n.breaks = 10) +
            scale_y_continuous(labels=scales::comma_format()) +
            theme_minimal() +
            labs(title = paste0("Trend in ", var),
                 subtitle = "Selected Census Tracts",
                 x = "Year \n(5-year trailing estimates)",
                 y = paste0("Estimate of ", var),
                 caption = "Source: American Community Survey.")
    })
    
    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    selected_GEOIDs <- reactiveValues(GEOIDs = NULL)
    
    ################################################# section two #################################################
    # base map
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles('CartoDB.Positron') %>%
            addPolygons(data = input_filtered_data(),
                        fillColor = "white",
                        fillOpacity = 0.3,
                        color = "hotpink",
                        weight = 2,
                        stroke = T,
                        layerId = as.character(input_filtered_data()$GEOID),
                        highlightOptions = highlightOptions(color = "mediumseagreen",
                                                            opacity = 1.0,
                                                            weight = 2,
                                                            bringToFront = TRUE)) %>%
            addDrawToolbar(
                targetGroup='Selected',
                polylineOptions=FALSE,
                markerOptions = FALSE,
                circleMarkerOptions = FALSE,
                circleOptions = FALSE,
                rectangleOptions = FALSE,
                polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.1,
                                                                                  color = 'black',
                                                                                  weight = 3)),
                editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })
    
    output$mytext <- renderText({
        validate(
            need(length(selected_GEOIDs$GEOIDs) >= 1, "Please select a valid shape in region bounds.")
        )
        
        summary <- input_filtered_data() %>% 
            summarize_tracts(tract_ids = selected_GEOIDs$GEOIDs)
        
        uni <- summary$universe
        var <- summary$variable
        sub <- summary$subgroup
        est <- summary$est
        moe <- summary$moe
        
        paste0("In the selected tracts in ", input$year, ", the sum of ", uni, " who are ", var, " for ", sub, " subgroups ", 
               " is ", round(est, 0), " +/- ", round(moe, 0), ".")
    })
    
    ############################################### section three #################################################
    observeEvent(input$mymap_draw_new_feature,{
        #Only add new layers for bounded locations
        found_in_bounds <- findLocations.sf(drawn_shape = input$mymap_draw_new_feature,
                                            reference_shape = msa.sf,
                                            reference_id_colname = "GEOID")
        
        for(id in found_in_bounds){
            if(id %in% data_of_click$clickedMarker){
                # don't add id
            } else {
                # add id
                data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
            }
        }
        
        # look up airports by ids found
        selected <- subset(input_filtered_data(), GEOID %in% data_of_click$clickedMarker)
        
        selected_GEOIDs$GEOIDs <- found_in_bounds
        
        leafletProxy("mymap") %>% 
            addPolygons(data = selected,
                        fillColor = "turquoise",
                        fillOpacity = 0.4,
                        color = "yellow",
                        weight = 3,
                        stroke = T,
                        layerId = as.character(selected$GEOID_selected),
                        highlightOptions = highlightOptions(color = "hotpink",
                                                            opacity = 1.0,
                                                            weight = 2,
                                                            bringToFront = TRUE))
        
        # browser()
    })
    
    
    
    ############################################### section four ##################################################
    observeEvent(input$mymap_draw_deleted_features,{
        # loop through list of one or more deleted features/ polygons
        for(feature in input$mymap_draw_deleted_features$features){
            
            # get ids for locations within the bounding shape
            bounded_layer_ids <- findLocations.sf(drawn_shape = feature,
                                                  reference_shape = msa.sf,
                                                  reference_id_colname = "GEOID")
            
            # remove second layer representing selected locations
            leafletProxy("mymap") %>% 
                removeShape(layerId = as.character(bounded_layer_ids))
            
            first_layer_ids <- subset(input_filtered_data(), GEOID_selected %in% bounded_layer_ids)$GEOID
            
            data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                       %in% first_layer_ids]
        }
        
        selected_GEOIDs$GEOIDs <- NULL
        
    })
}

# Run the application 
shinyApp(ui, server, options = list(height = 400))

# shiny::runApp(shinyApp(ui, server, options = list(height = 400)), display.mode = "showcase")
