library(shiny)
library(shinydashboard)
library(DT)  # Explicitly load DT package

shinyUI(fluidPage(
  titlePanel("Spotify Music Analysis Dashboard"),
  tags$head(
    tags$style(HTML("
      .logo {
        font-weight: bold;
        font-size: 24px;
        color: #1DB954;
        margin-bottom: 15px;
      }
      .well {
        background-color: #f8f9fa;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .tab-content {
        padding: 15px;
        background: white;
        border-radius: 5px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .nav-tabs {
        margin-bottom: 15px;
      }
      .dataTables_wrapper {
        margin-top: 10px;
      }
      .shiny-output-error {
        color: #dc3545;
        padding: 10px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "logo", icon("spotify"), "Spotify Analytics"),
      hr(),
      h4("Artist Filters"),
      sliderInput("followers_range", "Followers Range:",
                  min = 0, max = 100000000, 
                  value = c(0, 10000000), step = 100000,
                  pre = "Followers: "),
      sliderInput("artist_popularity", "Artist Popularity:",
                  min = 0, max = 100, value = c(30, 90)),
      selectInput("artist_genre", "Genre:",
                  choices = c("All", "pop", "rock", "hip hop", 
                              "jazz", "electronic", "r&b", "indie"),
                  selected = "All"),
      hr(),
      h4("Track Filters"),
      sliderInput("track_popularity", "Track Popularity:",
                  min = 0, max = 100, value = c(30, 90)),
      sliderInput("release_year", "Release Year:",
                  min = 1900, max = as.numeric(format(Sys.Date(), "%Y")),
                  value = c(2010, as.numeric(format(Sys.Date(), "%Y")))),
      sliderInput("duration_range", "Duration (minutes):",
                  min = 0, max = 10, value = c(1, 5), step = 0.5),
      hr(),
      h4("About"),
      p("This dashboard analyzes Spotify artist and track data."),
      p("Filter artists to see their tracks and audio features."),
      p("Version: 1.0.3")  # Updated version number
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Artists",
                 fluidRow(
                   column(8, DT::DTOutput("artist_table")),  # Explicit DT namespace
                   column(4, uiOutput("artist_details"))
                 ),
                 plotOutput("artist_scatter")
        ),
        tabPanel("Tracks",
                 fluidRow(
                   column(8, DT::DTOutput("track_table")),  # Explicit DT namespace
                   column(4, uiOutput("track_details"))
                 ),
                 fluidRow(
                   column(6, plotOutput("popularity_trend")),
                   column(6, plotOutput("duration_dist"))
                 ),
                 plotOutput("track_features")
        ),
        tabPanel("Help",
                 h3("How to use this dashboard"),
                 tags$ul(
                   tags$li("Use the filters in the sidebar to narrow down artists and tracks"),
                   tags$li("Click on an artist in the Artists tab to see their tracks"),
                   tags$li("Click on a track to see its audio features"),
                   tags$li("Explore trends in the Tracks tab")
                 ),
                 hr(),
                 h3("Data Description"),
                 tags$ul(
                   tags$li(strong("Artists data:"), "name, followers, genres, and popularity"),
                   tags$li(strong("Tracks data:"), "name, artists, audio features, and release date")
                 )
        )
      )
    )
  )
))