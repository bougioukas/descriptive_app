library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Understanding Location and Dispersion with Dot Plots (Integer Data)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dataset A"),
      sliderInput("mean_a", "Mean:", min = 0, max = 100, value = 50),
      sliderInput("sd_a", "Standard Deviation:", min = 1, max = 30, value = 5),
      
      hr(),
      
      h4("Dataset B"),
      sliderInput("mean_b", "Mean:", min = 0, max = 100, value = 50),
      sliderInput("sd_b", "Standard Deviation:", min = 1, max = 30, value = 15),
      
      hr(),
      
      sliderInput("n", "Sample Size (both datasets):", 
                  min = 10, max = 100, value = 30),
      
      actionButton("generate", "Generate New Data", class = "btn-primary"),
      actionButton("clear_a", "Clear Dataset A", class = "btn-warning"),
      actionButton("clear_b", "Clear Dataset B", class = "btn-warning"),
      
      # Button to toggle display of location statistics on the plot
      actionButton("show_location_stats_btn", "Toggle Location Values on Plot", class = "btn-info"),
      
      hr(),
      
      checkboxGroupInput("dispersion_viz", "Show Dispersion Methods:",
                         choices = c("Standard Deviation (±1 SD)" = "sd",
                                     "Standard Deviation (±2 SD)" = "sd2",
                                     "Interquartile Range (IQR)" = "iqr",
                                     "Range (Min-Max)" = "range"),
                         selected = c("sd", "iqr"))
    ),
    
    mainPanel(
      helpText("Click on the plots to add integer data points manually!"),
      plotOutput("dotplot_a", height = "280px", click = "plot_click_a"),
      plotOutput("dotplot_b", height = "280px", click = "plot_click_b"),
      
      hr(),
      
      fluidRow(
        column(6,
               h4("Dataset A Statistics"),
               verbatimTextOutput("stats_a")
        ),
        column(6,
               h4("Dataset B Statistics"),
               verbatimTextOutput("stats_b")
        )
      ),
      
      hr(),
      
      h4("Key Insights"),
      htmlOutput("insights")
    )
  )
)

# Helper function to calculate the mode (now works directly on integers)
get_mode <- function(v) {
  if (length(v) == 0) return(NA)
  uniqv <- unique(v)
  # Return the value that appears most frequently
  mode_result <- uniqv[which.max(tabulate(match(v, uniqv)))]
  
  # Check for multi-modal case (only return the lowest one for simplicity)
  if (length(mode_result) > 1) {
    return(min(mode_result))
  }
  return(mode_result)
}


server <- function(input, output, session) {
  
  # Initialize with integer data
  values <- reactiveValues(
    data_a = round(rnorm(30, 50, 5)),
    data_b = round(rnorm(30, 50, 15)),
    show_location_stats = FALSE 
  )
  
  # Toggle the visibility of the location stats labels
  observeEvent(input$show_location_stats_btn, {
    values$show_location_stats <- !values$show_location_stats
  })
  
  # Reactive data generation (on button click)
  observeEvent(input$generate, {
    # Ensure data is rounded to integers
    values$data_a <- round(rnorm(input$n, input$mean_a, input$sd_a))
    values$data_b <- round(rnorm(input$n, input$mean_b, input$sd_b))
  })
  
  # Reactive data generation (on slider change)
  observe({
    if (!is.null(input$mean_a) && !is.null(input$n)) {
      # Ensure data is rounded to integers
      values$data_a <- round(rnorm(input$n, input$mean_a, input$sd_a))
      values$data_b <- round(rnorm(input$n, input$mean_b, input$sd_b))
    }
  })
  
  # Add point to Dataset A on click (now rounds to integer)
  observeEvent(input$plot_click_a, {
    new_point <- round(input$plot_click_a$x)
    values$data_a <- c(values$data_a, new_point)
  })
  
  # Add point to Dataset B on click (now rounds to integer)
  observeEvent(input$plot_click_b, {
    new_point <- round(input$plot_click_b$x)
    values$data_b <- c(values$data_b, new_point)
  })
  
  # Clear Dataset A
  observeEvent(input$clear_a, {
    values$data_a <- numeric(0)
  })
  
  # Clear Dataset B
  observeEvent(input$clear_b, {
    values$data_b <- numeric(0)
  })
  
  create_dotplot <- function(data, title, color, xlim_range, show_viz, show_stats) {
    if (length(data) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 50, y = 1, label = "Click to add integer data points", size = 6) +
          scale_x_continuous(limits = xlim_range) +
          scale_y_continuous(limits = c(0.2, 2.0)) +
          labs(title = title, x = "Value", y = "") +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.title = element_text(size = 14, face = "bold")
          )
      )
    }
    
    df <- data.frame(x = data, y = 1)
    # Add a little jitter for better visibility on the dot plot
    df$y <- df$y + runif(nrow(df), -0.15, 0.15)
    
    mean_val <- mean(data)
    sd_val <- sd(data)
    median_val <- median(data)
    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)
    mode_val <- get_mode(data) # Calculate mode
    
    p <- ggplot(df, aes(x = x, y = y)) +
      # 1. Plot the actual data points
      geom_point(size = 4, alpha = 0.7, color = color) +
      
      # 2. Add Mean (Red Dashed Line)
      geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", size = 1.2) +
      
      # 3. Add Mean (Circle) marker (Shape 16) - ALWAYS VISIBLE
      geom_point(data = data.frame(x = mean_val, y = 1.1),
                 aes(x = x, y = y),
                 shape = 16, size = 6, color = "red") +
      
      # Set plot limits and themes
      scale_y_continuous(limits = c(0.2, 2.0)) +
      scale_x_continuous(limits = xlim_range) +
      labs(title = title, x = "Value", y = "") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12)
      )
    
    # Conditional display of location statistics markers, lines, and labels
    if (show_stats) {
      
      # --- Median (Green Triangle) ---
      p <- p + 
        # Add marker
        geom_point(data = data.frame(x = median_val, y = 1.25),
                   aes(x = x, y = y),
                   shape = 17, size = 6, color = "darkgreen") +
        # Add label
        annotate("text", x = median_val, y = 1.8,
                 label = paste0("Median: ", round(median_val, 2)),
                 color = "darkgreen", fontface = "bold", size = 4, vjust = -0.5)
      
      # --- Mode (Violet Square) ---
      if (!is.na(mode_val)) {
        p <- p + 
          # Add marker
          geom_point(data = data.frame(x = mode_val, y = 1.4),
                     aes(x = x, y = y),
                     shape = 15, size = 6, color = "darkviolet") +
          # Add dotted line for Mode
          geom_vline(xintercept = mode_val, color = "darkviolet", linetype = "dotted", size = 1.2, alpha = 0.5) +
          # Add label for Mode
          annotate("text", x = mode_val, y = 1.55,
                   label = paste0("Mode: ", round(mode_val, 2)),
                   color = "darkviolet", fontface = "bold", size = 4, vjust = -0.5)
      }
      
      # --- Mean Label (placed here for consistency in toggling labels) ---
      p <- p + annotate("text", x = mean_val, y = 1.5,
                        label = paste0("Mean: ", round(mean_val, 2)),
                        color = "red", fontface = "bold", size = 4, vjust = -0.5)
    }
    
    # Range visualization (Dispersion) - These remain independent of the show_stats toggle
    if ("range" %in% show_viz) {
      p <- p + 
        annotate("segment", x = min(data), xend = max(data), 
                 y = 0.25, yend = 0.25, color = "purple", size = 1.5,
                 arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
        annotate("text", x = (min(data) + max(data))/2, y = 0.25, 
                 label = "Range", vjust = -0.5, color = "purple", 
                 fontface = "bold", size = 3.5)
    }
    
    # IQR visualization (Dispersion) - These remain independent of the show_stats toggle
    if ("iqr" %in% show_viz) {
      # Add a solid line for the median to accompany the IQR visual
      p <- p + geom_vline(xintercept = median_val, color = "darkgreen", linetype = "solid", size = 1.2, alpha = 0.5) +
        annotate("segment", x = q1, xend = q3,
                 y = 1.6, yend = 1.6, color = "blue", size = 1.5,
                 arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
        annotate("text", x = (q1 + q3)/2, y = 1.6, label = "IQR", 
                 vjust = -0.5, color = "blue", fontface = "bold", size = 3.5)
    }
    
    # SD ±2 with arrows (Dispersion)
    if ("sd2" %in% show_viz) {
      p <- p + 
        annotate("segment", x = mean_val - 2*sd_val, xend = mean_val + 2*sd_val,
                 y = 0.4, yend = 0.4, color = "orange", size = 1.5,
                 arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
        annotate("text", x = mean_val, y = 0.4, label = "±2 SD", 
                 vjust = -0.5, color = "orange", fontface = "bold", size = 3.5)
    }
    
    # SD ±1 with arrows (Dispersion)
    if ("sd" %in% show_viz) {
      arrow_y <- if ("sd2" %in% show_viz) 0.55 else 0.4 
      
      p <- p + 
        annotate("segment", x = mean_val - sd_val, xend = mean_val + sd_val,
                 y = arrow_y, yend = arrow_y, color = "darkorange", size = 1.5,
                 arrow = arrow(ends = "both", length = unit(0.15, "inches"))) +
        annotate("text", x = mean_val, y = arrow_y, label = "±1 SD", 
                 vjust = -0.5, color = "darkorange", fontface = "bold", size = 3.5)
    }
    
    p
  }
  
  output$dotplot_a <- renderPlot({
    all_data <- c(values$data_a, values$data_b)
    if (length(all_data) < 2) {
      xlim_range <- c(0, 100)
    } else {
      xlim_range <- c(min(all_data) - 5, max(all_data) + 5)
    }
    
    # Pass the show_location_stats state to the plotting function
    create_dotplot(values$data_a, "Dataset A (click to add points)", "steelblue", xlim_range, input$dispersion_viz, values$show_location_stats)
  })
  
  output$dotplot_b <- renderPlot({
    all_data <- c(values$data_a, values$data_b)
    if (length(all_data) < 2) {
      xlim_range <- c(0, 100)
    } else {
      xlim_range <- c(min(all_data) - 5, max(all_data) + 5)
    }
    
    # Pass the show_location_stats state to the plotting function
    create_dotplot(values$data_b, "Dataset B (click to add points)", "coral", xlim_range, input$dispersion_viz, values$show_location_stats)
  })
  
  output$stats_a <- renderText({
    data <- values$data_a
    if (length(data) == 0) return("No data yet. Click on the plot to add points!")
    if (length(data) == 1) return(paste0("Single point at: ", round(data[1], 2), "\nAdd more points for statistics."))
    
    mode_val <- get_mode(data) # Get mode
    
    paste0(
      "n = ", length(data), " points\n\n",
      "Location:\n",
      "  Mean (Red Circle): ", round(mean(data), 2), "\n",
      "  Median (Green Triangle): ", round(median(data), 2), "\n",
      "  Mode (Violet Square): ", if(is.na(mode_val)) "N/A" else round(mode_val, 2), "\n\n", 
      "Dispersion:\n",
      "  Range:    ", round(max(data) - min(data), 2), "\n",
      "  IQR:      ", round(IQR(data), 2), "\n",
      "  SD:       ", round(sd(data), 2), "\n",
      "  Variance: ", round(var(data), 2)
    )
  })
  
  output$stats_b <- renderText({
    data <- values$data_b
    if (length(data) == 0) return("No data yet. Click on the plot to add points!")
    if (length(data) == 1) return(paste0("Single point at: ", round(data[1], 2), "\nAdd more points for statistics."))
    
    mode_val <- get_mode(data) # Get mode
    
    paste0(
      "n = ", length(data), " points\n\n",
      "Location:\n",
      "  Mean (Red Circle): ", round(mean(data), 2), "\n",
      "  Median (Green Triangle): ", round(median(data), 2), "\n",
      "  Mode (Violet Square): ", if(is.na(mode_val)) "N/A" else round(mode_val, 2), "\n\n", 
      "Dispersion:\n",
      "  Range:    ", round(max(data) - min(data), 2), "\n",
      "  IQR:      ", round(IQR(data), 2), "\n",
      "  SD:       ", round(sd(data), 2), "\n",
      "  Variance: ", round(var(data), 2) 
    )
  })
  
  output$insights <- renderUI({
    mean_diff <- abs(input$mean_a - input$mean_b)
    sd_diff <- abs(input$sd_a - input$sd_b)
    
    location_similar <- mean_diff < 5
    dispersion_similar <- sd_diff < 3
    
    base_html <- "
      <p><strong>Visual Key:</strong></p>
      <ul>
        <li><span style='color:red; font-size:1.2em;'>●</span> <strong>Mean (Red Circle/Dashed Line)</strong>: Always visible. The center of mass.</li>
        <li><span style='color:darkgreen; font-size:1.2em;'>▲</span> <strong>Median (Green Triangle)</strong>: Appears with **Toggle Location** button. The exact middle point of the data.</li>
        <li><span style='color:darkviolet; font-size:1.2em;'>■</span> <strong>Mode (Violet Square/Dotted Line)</strong>: Appears with **Toggle Location** button. The most frequent whole number.</li>
      </ul>
      <p><strong>Dispersion Visualization Methods:</strong></p>
      <ul>
        <li><strong>Range:</strong> Distance from minimum to maximum (purple line at bottom)</li>
        <li><strong>IQR (Interquartile Range):</strong> Blue box showing middle 50% of data (median solid line appears here)</li>
        <li><strong>Standard Deviation (±1 SD):</strong> Orange box covering $\\approx 68\\%$ of data</li>
        <li><strong>Standard Deviation (±2 SD):</strong> Lighter orange showing $\\approx 95\\%$ of data</li>
      </ul>
      <hr>"
    
    if (location_similar && !dispersion_similar) {
      message <- "<p><strong>Comparing DISPERSION:</strong></p>
                  <ul>
                    <li>Both datasets are centered at a similar **location** (the red circles are close).</li>
                    <li>But the Dataset with larger standard deviation has points much more **scattered**.</li>
                  </ul>"
    } else if (!location_similar && dispersion_similar) {
      message <- "<p><strong>Comparing LOCATION:</strong></p>
                  <ul>
                    <li>Datasets are centered at different **locations** (the red circles are separated).</li>
                    <li>But they have similar **spread** (dispersion).</li>
                  </ul>"
    } else if (!location_similar && !dispersion_similar) {
      message <- "<p><strong>Both LOCATION and DISPERSION differ:</strong></p>
                  <ul>
                    <li>Datasets differ in both center and spread.</li>
                  </ul>"
    } else {
      message <- "<p><strong>Very similar distributions:</strong></p>
                  <ul>
                    <li>Both location and dispersion are similar.</li>
                    <li>Try adding an outlier to see how the **red circle (mean)** shifts relative to the **green triangle (median)**.</li>
                  </ul>"
    }
    
    HTML(paste0(base_html, message))
  })
}

shinyApp(ui = ui, server = server)