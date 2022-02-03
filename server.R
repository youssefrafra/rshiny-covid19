#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


source("global.R")

shinyServer(function(input, output) {

  # Total Cases
    output$total_cases  <- shinydashboard::renderValueBox({
      x <- WHO_COVID %>% filter(Name == input$select_country) %>% select(Cases_cumulative_total)
      valueBox( x, h2("Total Cases"), icon = icon("viruses"), color = "green")
    })
    # 
    # Newly reported Cases in last 24 hours
    output$new_cases  <- renderValueBox({
      x <- WHO_COVID %>% filter(Name == input$select_country) %>% select(Cases_newly_reported_in_last_24_hours)
      valueBox(x[[1]], h2("Newly reported Cases in last 24 hours"), icon = icon("virus"), color = "yellow")
    })
    # 
    # Total Deaths
    output$total_deaths  <- renderValueBox({
      x <- WHO_COVID %>% filter(Name == input$select_country) %>% select(Deaths_cumulative_total)
      valueBox(x , h2("Total Deaths"), icon = icon("skull-crossbones"), color = "orange")
    })
    # 
    # Newly reported Deaths in last 24 hours
    output$new_deaths  <- renderValueBox({
      x <- WHO_COVID %>% filter(Name == input$select_country) %>% select(`Deaths_newly_reported_in last_24_hours`)
      valueBox(x , h2("Newly reported Deaths in last 24 hours"), icon = icon("skull"), color ="red")
    })
    # 
    # Total vaccinations
    output$total_vac <- renderValueBox({
      x <- vaccination_data %>% filter(Name == input$select_country) %>% select(PERSONS_FULLY_VACCINATED)
      if(input$select_country == 'Global'){
        x <- vaccination_data %>% summarize(total = sum(PERSONS_FULLY_VACCINATED, na.rm=TRUE)) %>% select(total)
      }
      valueBox(x , h2("Total fully Vaccinated"), icon = icon("syringe"), color ="blue")
    })
    # 
    # Newly reported Deaths in last 24 hours
     test <- reactive({input$select_country != 'Global'})
      output$vac_used  <- renderValueBox({
        if(test()){
          types <- vaccination_data %>% 
                    filter(Name == input$select_country) %>% 
                    select(VACCINES_USED) %>% 
                    str_replace_all(',', "</br>")
          x <- HTML(paste(types))
          valueBox(x , h2("Vaccine used"), icon = icon("syringe"), color ="lime")
        }else{
          first_countries <- vaccination_data %>% 
                        arrange(FIRST_VACCINE_DATE) %>% 
                        select(Name, FIRST_VACCINE_DATE) %>% 
                        head(1)
          # first_countries <-
          x <- HTML(paste("Country :",first_countries[,1],collapse = ", "))
          valueBox(x , h2("First country to use a vaccine"), icon = icon("trophy"), color ="lime")
        }
      })
    # 
    # Bar Plot 1
    output$plot1 <- renderPlotly({
      fig <- plot_ly(WHO_COVID %>% 
                       filter(Name != "Global") %>% 
                       arrange(desc(Cases_cumulative_total)) %>%
                       head(10), 
                     x= ~Name, 
                     y= ~Cases_cumulative_total, 
                     type = 'bar') %>%
              layout(
              title = "Global Cases",
              xaxis = list(title = "Country",
                           categoryorder = "array",
                           categoryarray = ~Name),
              yaxis = list(title = "Cases",
              autosize = F, 
              width = 400, 
              height = 400)
            )
      fig
    })
    # 
    # Bar Plot 2
    output$plot2 <- renderPlotly({
      fig <- plot_ly(vaccination_data %>% 
                       arrange(desc(TOTAL_VACCINATIONS)) %>% 
                       head(10), 
                     x= ~Name, 
                     y= ~TOTAL_VACCINATIONS, 
                     type = 'bar') %>%
        layout(
          title = "Total vaccination for each country",
          xaxis = list(title = "Country",
                       categoryorder = "array",
                       categoryarray = ~Name),
          yaxis = list(title = "# of vaccinated",
                       autosize = F, 
                       width = 400, 
                       height = 400)
        )
      fig
    })
    # 
    # Bar Plot 3
    output$plot3 <- renderPlotly({
      fig <- plot_ly(vaccination_data %>% 
                       arrange(desc(PERSONS_FULLY_VACCINATED_PER100)) %>% 
                       head(10), 
                     x= ~Name, 
                     y= ~PERSONS_FULLY_VACCINATED_PER100, 
                     type = 'bar') %>%
        layout(
          title = "Percent vaccinated for each country",
          xaxis = list(title = "Country",
                       categoryorder = "array",
                       categoryarray = ~Name),
          yaxis = list(title = "% of vaccinated",
                       autosize = F, 
                       width = 400, 
                       height = 400)
        )
      fig
    })
    #
    # Map plot
    output$map1 <- renderLeaflet({
      mybins <- seq(4, 6.5, by=0.5)
      mypalette <- colorBin( palette="YlOrBr", 
                             domain=vaccines_with_coords$TOTAL_VACCINATIONS, 
                             na.color="transparent", 
                             bins=mybins)
      
      leaflet(vaccines_with_coords) %>%
        addTiles() %>%
        setView(lng = 45,
                lat = 40,
                zoom = 3.5) %>%
        addMarkers(lng=vaccines_with_coords$longitude, lat=vaccines_with_coords$latitude, popup = paste0("<b>Country: </b>",
                                                                                                  vaccines_with_coords$Name,
                                                                                                 "</br>",
                                                                                                 "<b>Total Vaccinations: </b>",
                                                                                                 vaccines_with_coords$TOTAL_VACCINATIONS,
                                                                                                 "</br>",
                                                                                                 "<b>Vaccines used: </b>",
                                                                                                 vaccines_with_coords$VACCINES_USED,
                                                                                                 "</br>",
                                                                                                 "<b>Data Source: </b>",
                                                                                                 vaccines_with_coords$DATA_SOURCE,
                                                                                                 "</br>",
                                                                                                 "<b>First vaccine date: </b>",
                                                                                                 vaccines_with_coords$FIRST_VACCINE_DATE)) %>%
        addCircles(radius = (vaccines_with_coords$TOTAL_VACCINATIONS)/3000, 
                   weight = 1, 
                   color = vaccines_with_coords$TOTAL_VACCINATIONS,
                   fillColor = vaccines_with_coords$TOTAL_VACCINATIONS, 
                   fillOpacity = 0.7, 
        ) 
    })
    # 
    # Map2 plot
    output$map2 <- renderLeaflet({
      customIcon <- makeIcon(
                      iconUrl = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRytdyS7qcBYr0ThIz75CfsjTntY6kra_Hiqw&usqp=CAU",
                      iconWidth = 30, iconHeight = 30
                    )
      leaflet(data_with_coords) %>%
        addTiles() %>%
        setView(lng = 45,
                lat = 40,
                zoom = 3.5) %>%
        # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
        addMarkers(lng=data_with_coords$longitude, lat=data_with_coords$latitude, popup = paste0("<b>Country: </b>",
                                                                                                 data_with_coords$Name,
                                                                                                 "</br>",
                                                                                                 "<b>Total deaths: </b>",
                                                                                                 data_with_coords$Deaths_cumulative_total,
                                                                                                 "</br>",
                                                                                                 "<b>Total Cases: </b>",
                                                                                                 data_with_coords$Cases_cumulative_total,
                                                                                                 "</br>",
                                                                                                 "<b>Total deaths: </b>",
                                                                                                 data_with_coords$Deaths_cumulative_total)) %>%
        addCircles(radius = data_with_coords$Deaths_cumulative_total, 
                   weight = 1, 
                   color = "#777777",
                   fillColor = data_with_coords$Deaths_cumulative_total, 
                   fillOpacity = 0.7, 
        )
    })

})
