library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)

# Load data with error handling
artists <- tryCatch({
  read.csv("artists.csv") %>%
    mutate(genres = as.character(genres))
}, error = function(e) {
  stop("Failed to load artist data: ", e$message)
})

tracks <- tryCatch({
  read.csv("tracks.csv") %>%
    mutate(
      release_date = as.Date(release_date),
      release_year = as.numeric(format(release_date, "%Y")), # Using base R instead of lubridate
      duration_min = duration_ms / 60000,
      explicit = as.factor(explicit),
      artists = as.character(artists),
      id_artists = as.character(id_artists)
    )
}, error = function(e) {
  stop("Failed to load tracks data: ", e$message)
})

shinyServer(function(input, output, session) {
  
  # Filter artists data
  filtered_artists <- reactive({
    req(input$followers_range, input$artist_popularity)
    
    data <- artists
    if (input$artist_genre != "All") {
      data <- data[grepl(input$artist_genre, data$genres, ignore.case = TRUE), ]
    }
    data <- data %>%
      filter(
        followers >= input$followers_range[1],
        followers <= input$followers_range[2],
        popularity >= input$artist_popularity[1],
        popularity <= input$artist_popularity[2]
      )
    data
  })
  
  # Filter tracks data
  filtered_tracks <- reactive({
    req(input$track_popularity, input$release_year, input$duration_range)
    
    s <- input$artist_table_rows_selected
    if (length(s)) {
      selected_artist <- filtered_artists()[s, ]
      tracks <- tracks[grepl(selected_artist$id, tracks$id_artists), ]
    }
    
    tracks %>%
      filter(
        popularity >= input$track_popularity[1],
        popularity <= input$track_popularity[2],
        release_year >= input$release_year[1],
        release_year <= input$release_year[2],
        duration_min >= input$duration_range[1],
        duration_min <= input$duration_range[2]
      )
  })
  
  # Artist data table
  output$artist_table <- DT::renderDataTable({
    datatable(
      filtered_artists() %>% select(name, followers, genres, popularity),
      selection = 'single',
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Tracks data table
  output$track_table <- DT::renderDataTable({
    datatable(
      filtered_tracks() %>% 
        select(name, artists, popularity, release_year, duration_min, danceability),
      selection = 'single',
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Artist popularity vs followers scatter plot
  output$artist_scatter <- renderPlot({
    req(nrow(filtered_artists()) > 0)
    
    ggplot(filtered_artists(), aes(x = followers, y = popularity, 
                                   size = followers, color = popularity)) +
      geom_point(alpha = 0.7) +
      scale_x_log10(labels = scales::comma) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(
        title = "Artist Popularity vs Followers",
        x = "Followers (log scale)", 
        y = "Popularity"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Track features radar chart (simplified to bar plot)
  output$track_features <- renderPlot({
    s <- input$track_table_rows_selected
    req(length(s) > 0)
    
    track <- filtered_tracks()[s, ]
    features <- track %>% 
      select(danceability, energy, speechiness, 
             acousticness, instrumentalness, liveness) %>%
      pivot_longer(everything(), names_to = "feature", values_to = "value")
    
    ggplot(features, aes(x = feature, y = value, fill = feature)) +
      geom_col() +
      coord_polar() +
      labs(title = "Track Audio Features") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
      )
  })
  
  # Track popularity over time
  output$popularity_trend <- renderPlot({
    req(nrow(filtered_tracks()) > 0)
    
    yearly_data <- filtered_tracks() %>%
      group_by(release_year) %>%
      summarise(avg_popularity = mean(popularity, na.rm = TRUE))
    
    ggplot(yearly_data, aes(x = release_year, y = avg_popularity)) +
      geom_line(color = "steelblue", linewidth = 1.5) +
      geom_point(color = "steelblue", size = 3) +
      labs(
        title = "Average Track Popularity by Release Year",
        x = "Release Year", 
        y = "Average Popularity"
      ) +
      theme_minimal()
  })
  
  # Track duration distribution
  output$duration_dist <- renderPlot({
    req(nrow(filtered_tracks()) > 0)
    
    ggplot(filtered_tracks(), aes(x = duration_min)) +
      geom_histogram(bins = 20, fill = "purple", alpha = 0.7) +
      labs(
        title = "Track Duration Distribution",
        x = "Duration (minutes)", 
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # Artist details
  output$artist_details <- renderUI({
    s <- input$artist_table_rows_selected
    if (length(s)) {
      artist <- filtered_artists()[s, ]
      tagList(
        h3(artist$name),
        hr(),
        p(strong("Followers:"), format(artist$followers, big.mark = ",")),
        p(strong("Popularity:"), artist$popularity),
        p(strong("Genres:"), strsplit(gsub("[\\[\\]']", "", artist$genres), ", ")[[1]] %>% 
            paste(collapse = ", "))
      )
    } else {
      tagList(
        h3("No artist selected"),
        p("Click on an artist in the table to see details")
      )
    }
  })
  
  # Track details
  output$track_details <- renderUI({
    s <- input$track_table_rows_selected
    if (length(s)) {
      track <- filtered_tracks()[s, ]
      tagList(
        h3(track$name),
        hr(),
        p(strong("Artist(s):"), gsub("[\\[\\]']", "", track$artists)),
        p(strong("Popularity:"), track$popularity),
        p(strong("Release Date:"), track$release_date),
        p(strong("Duration:"), round(track$duration_min, 2), "minutes"),
        p(strong("Explicit:"), ifelse(track$explicit == 1, "Yes", "No"))
      )
    } else {
      tagList(
        h3("No track selected"),
        p("Click on a track in the table to see details")
      )
    }
  })
})
