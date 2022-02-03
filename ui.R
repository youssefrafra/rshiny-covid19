#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.R")
data_with_coords <- merge(x = WHO_COVID, y = country, by = "Name", all.y  = TRUE)
shinyUI(dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Global Covid"),
  dashboardSidebar(
    sidebarMenu(
      status="primary",
      h4('Selected Country'),
      
      selectInput('select_country',
                  "Country:",
                  WHO_COVID[1],
                  selected = NULL,
                  multiple = FALSE,
                  selectize = TRUE,
                  width = "100%",
                  size = NULL),
      
      hr(),
      

      menuItem("Main", tabName = "dashboard", icon = icon("home")),
      menuItem("Graphs", tabName = "Plot", icon = icon("chart-bar")),
      menuItem("Map of vaccinations", tabName = "Map", icon = icon("map-marked-alt")),
      menuItem("Map of cases", tabName = "Map2", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    div(style='padding:5px;',
        tabItems(
          
          tabItem("dashboard",
                  
                  fluidRow(
                    h1('Cases and Deaths'),
                    column(6,
                           valueBoxOutput("total_cases",  width = 12),
                           valueBoxOutput("new_cases",      width = 12)
                    ),
                    column(6,
                           valueBoxOutput("total_deaths", width = 12),
                           valueBoxOutput("new_deaths",     width = 12)
                    ) 
                  ),# close fluidRow
                  fluidRow(
                    h1('Total vaccinations and type of vaccine used'),
                    column(6,
                           valueBoxOutput("total_vac",  width = 12),
                           valueBoxOutput("vac_used",   width = 12)
                    ),
                    column(6,
                           valueBoxOutput("", width = 12),
                           valueBoxOutput("", width = 12)
                    ) 
                  ) # close fluidRow
          ), # close tabItem
          tabItem("Plot",
                  h1('Top 10 Countries with most Covid-19 Cases'),
                  plotlyOutput("plot1"),
                  h1('Top 10 countries with most vaccinated population'),
                  plotlyOutput("plot2")
                  h1('Top 10 countries with highest percent of vaccinated population'),
                  plotlyOutput("plot3")
          ), # close tabItem
          tabItem("Map",
                  leafletOutput("map1",height = "900px")),
          tabItem("Map2",
                  leafletOutput("map2",height = "900px"))
        ) # close tabItems
    ) # close div
  ) # close dashboardBody()
))
