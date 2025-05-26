library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

# Load data
artists <-read.csv("artists.csv") %>%
    mutate(genres = as.character(genres))

tracks <- read.csv("tracks.csv") %>%
    mutate(
      release_date = as.Date(release_date),
      release_year = as.numeric(format(release_date, "%Y")),
      duration_min = duration_ms / 60000,
      explicit = as.factor(explicit),
      artists = as.character(artists),
      id_artists = as.character(id_artists)
    )

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
  
  # Combined artist and track popularity plot
  output$artist_scatter <- renderPlotly({
    req(nrow(filtered_artists()) > 0)
    
    # Prepare track data with song names
    track_data <- filtered_tracks() %>%
      mutate(id_artists = gsub("\\[|\\]|'", "", id_artists)) %>%
      separate_rows(id_artists, sep = ",\\s*")
    
    # Calculate average track popularity per artist
    avg_tracks <- track_data %>%
      group_by(id_artists) %>%
      summarise(
        avg_track_pop = mean(popularity, na.rm = TRUE),
        track_count = n(),
        track_names = paste(name, collapse = "<br>")  # Combine all track names
      )
    
    # Join with artist data
    plot_data <- filtered_artists() %>%
      mutate(id_clean = gsub("\\[|\\]|'", "", id)) %>%
      left_join(avg_tracks, by = c("id_clean" = "id_artists")) %>%
      filter(!is.na(avg_track_pop))
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No matching tracks found for these artists",
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Create plotly visualization
    p <- plot_ly(
      data = plot_data,
      x = ~popularity,
      y = ~avg_track_pop,
      type = "scatter",
      mode = "markers",
      size = ~followers,
      color = ~track_count,
      colors = c("blue", "red"),
      hoverinfo = "text",
      text = ~paste(
        "<b>Artist:</b> ", name, "<br>",
        "<b>Popularity:</b> ", popularity, "<br>",
        "<b>Avg Track Popularity:</b> ", round(avg_track_pop, 1), "<br>",
        "<b>Followers:</b> ", format(followers, big.mark = ","), "<br>",
        "<b>Track Count:</b> ", track_count, "<br>",
        "<b>Tracks:</b> <br>", track_names
      ),
      marker = list(
        sizemode = "diameter",
        opacity = 0.7,
        line = list(width = 0)
      )
    ) %>%
      layout(
        title = "Artist Popularity vs Average Track Popularity",
        xaxis = list(title = "Artist Popularity"),
        yaxis = list(title = "Average Track Popularity"),
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        )
      )
    
    p
  })
    
  # Download handler for scatter plot
  output$download_scatter <- downloadHandler(
    filename = function() {
      paste("artist_track_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      combined_data <- filtered_artists() %>%
        left_join(
          filtered_tracks() %>%
            separate_rows(id_artists, sep = ", ") %>%
            group_by(id_artists) %>%
            summarise(
              avg_track_popularity = mean(popularity, na.rm = TRUE),
              track_count = n()
            ),
          by = c("id" = "id_artists")
        ) %>%
        filter(!is.na(avg_track_popularity))
      
      p <- ggplot(combined_data, aes(x = popularity, y = avg_track_popularity, 
                                     size = followers, color = track_count)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
        scale_color_gradient(low = "blue", high = "red", name = "Number of Tracks") +
        scale_size_continuous(name = "Followers") +
        labs(
          title = "Artist Popularity vs Average Track Popularity",
          subtitle = "Size represents followers, color represents number of tracks",
          x = "Artist Popularity", 
          y = "Average Track Popularity"
        ) +
        theme_minimal()
      
      ggsave(file, plot = p, device = "png", width = 10, height = 6, dpi = 300)
    }
  )
  
  
  
  
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
  output$popularity_trend <- renderPlotly({
    req(nrow(filtered_tracks()) > 0)
    
    yearly_data <- filtered_tracks() %>%
      group_by(release_year) %>%
      summarise(avg_popularity = mean(popularity, na.rm = TRUE))
    
    plot_ly(
      data = yearly_data,
      x = ~release_year,
      y = ~avg_popularity,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'steelblue', width = 1.5),
      marker = list(color = 'steelblue', size = 8),
      hoverinfo = 'text',
      text = ~paste(
        '<b>Year:</b>', release_year,
        '<br><b>Avg Popularity:</b>', round(avg_popularity, 1)
      )
    ) %>%
      layout(
        title = list(text = "<b>Average Track Popularity by Release Year</b>", y = 0.95),
        xaxis = list(title = "Release Year", gridcolor = 'lightgray'),
        yaxis = list(title = "Average Popularity", gridcolor = 'lightgray'),
        plot_bgcolor = 'white',
        hoverlabel = list(bgcolor = 'white', font = list(size = 12))
      ) %>%
      config(displayModeBar = TRUE)
  })
    
  # Track duration distribution
  output$duration_dist <- renderPlotly({
    req(nrow(filtered_tracks()) > 0)
    
    plot_ly(data = filtered_tracks(),
            x = ~duration_min,
            type = "histogram",
            nbinsx = 20,
            marker = list(
              color = "rgba(128, 0, 128, 0.7)",
              line = list(color = "white", width = 1)
            ),
            hoverinfo = "y") %>%  # This shows only the count on hover
      layout(
        title = "Track Duration Distribution",
        xaxis = list(title = "Duration (minutes)"),
        yaxis = list(title = "Count"),
        bargap = 0.1
      )
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