#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic

shinyServer(
  function(input, output, session) {
    ##### Base File Update #####
    
    update_function <- reactive({
      req(input$base_file)  # Ensure the file input is not null
      
      
      temp_file <- paste0(update_path$datapath, ".xlsx")
      file.rename(update_path$datapath, temp_file)
      
      # Attempt to read the Excel file
      new_dat <- tryCatch({
        read_excel(
          path = temp_file,
          col_names = TRUE,
          col_types = c(
            rep("text", 14),
            rep("numeric", 3),
            "date",
            "text",
            "text",
            "numeric",
            "numeric",
            "text",
            rep("skip", 10)
          )
        )
      }, error = function(e) {
        showNotification("Error reading the Excel file!", type = "error")
        NULL
      })
      
      # Proceed only if data was read successfully
      if (!is.null(new_dat)) {
        new_dat <- tryCatch({
          data.base.function(new_dat)
        }, error = function(e) {
          showNotification("Error processing the data!", type = "error")
          NULL
        })
      }
      
      return(new_dat)
      
    })
    
    output$download_file <- renderUI({
      if (!is.null(update_function()))
        downloadButton(outputId = "download_new_dat", label = "Download")
    })
    
    output$download_new_dat <- downloadHandler(
      filename = function() {
        paste0("database.xlsx")
      },
      content = function(file) {
        req(update_function())  # Ensure data is available before proceeding
        openxlsx::write.xlsx(update_function(), file, sheetName = "data")
      }
    )
    
    ##### Distance Table #####
    
    # Function to find euclidean distance in a 3d graph.
    euc <- function(x, y) {
      sqrt(sum((x - y) ^ 2))
    }
    
    # Data Filters
    datChanges <- reactive({
      errorFilter <- input$errorFilter
      ConstFilter <- input$ConstFilter
      typeFilter <- input$typeFilter
      
      # Removes any potential NAs form Updated data file
      dat <- dat[!is.na(dat$SuspectedError), ]
      
      ind <- (dat$SuspectedError %in% errorFilter) &
        (dat$`Main constituent for DB` %in% ConstFilter) &
        (dat$`Type for DB` %in% typeFilter)
      
      datChanges <- dat[ind, ]
      
      return(datChanges)
    })
    
    # Euclidean Reactive Distance Table
    tableEuclidean <- reactive({
      req(datChanges())
      
      dat <- datChanges()
      
      if (input$Datatype == "0") {
        # the given observations.
        obs <- c(input$x1, input$x2, input$x3)
        
        # Check for missing values in obs
        if (any(is.na(obs))) {
          stop("Some input values (x1, x2, x3) are missing. Please enter valid numbers.")
        }
        
        # Extract and clean database observations
        datobs <- cbind(dat$`208/204Pb`, dat$`207/204Pb`, dat$`206Pb/204Pb`) %>%
          na.omit(datobs)  # Remove rows with NA values
        
        # Compute Euclidean distances
        distance <- apply(
          X = datobs,
          MARGIN = 1,
          FUN = euc,
          x = obs
        )
        
        # Determine how many closest observations to return
        numobs <- min(input$numobs, length(distance))
        
        # Find the closest observations
        place <- order(distance, decreasing = FALSE)[1:numobs]
        
        # Ensure `dat` is properly subsetted (only include complete cases)
        tabeuc <- cbind(distance, dat)[place, ]
        
        # Print the selected observations
        print(tabeuc)
        
      } else {
        # In case the columns are given in the wrong order.
        # Ensure the correct column indices are selected
        clnm <- as.numeric(c(input$H1, input$H2, input$H3))
        
        # Validate input file
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        # Read CSV file
        tab <- read.csv(inFile$datapath)
        
        # Reorder columns based on user input
        tab1 <- cbind(
          "208/204" = tab[, clnm[1]],
          "207/204" = tab[, clnm[2]],
          "206/204" = tab[, clnm[3]]
        )
        
        # Extract sample IDs
        ids <- as.character(tab[, 1])
        
        # this function copies the process of a single observation.
        eucTable <- function(vec) {
          datobs <- cbind(dat$`208/204Pb`, dat$`207/204Pb`, dat$`206Pb/204Pb`)
          
          # Handle missing values
          datobs <- na.omit(datobs)
          
          # Compute Euclidean distances
          distance <- apply(
            X = datobs,
            MARGIN = 1,
            FUN = euc,
            x = vec
          )
          
          # Ensure valid numobs value
          numobs <- min(input$numobs, length(distance))
          
          # Find closest observations
          place <- order(distance, decreasing = FALSE)[1:numobs]
          
          return(cbind(distance, dat)[place, ])
          
        }
        
        # Apply function to all observations using lapply (faster than for-loop)
        tabeuc_list <- lapply(seq_len(nrow(tab1)), function(i) {
          obs <- tab1[i, ]
          tmp <- cbind("ID" = rep(ids[i], input$numobs), eucTable(obs))
          return(tmp)
        })
        
        # Combine results into a single data frame
        tabeuc <- do.call(rbind, tabeuc_list)
        
        # Print final table
        print(tabeuc)
      }
      
    })
    
    ##### Countries and Regions Filter #####
    
    # The code that allows to update the country and region filters.
    observeEvent(input$update, {
      # if one observation is given.
      tabeuc <- tableEuclidean()
      
      updateCheckboxGroupInput(session,
                               inputId = "selectCountry",
                               selected = dat$Country[dat$Country %in% tabeuc$Country])
      
      updateCheckboxGroupInput(session, inputId = "Reg", selected = dat$Region[dat$Region %in% tabeuc$Region])
      
    })
    
    # Buttons to select and de-select all countries.
    observeEvent(input$checkCountryAll, {
      updateCheckboxGroupInput(session,
                               inputId = "selectCountry",
                               selected = unique(dat$Country))
    })
    observeEvent(input$checkCountryNone, {
      updateCheckboxGroupInput(session, inputId = "selectCountry", selected = NA)
    })
    
    observeEvent(input$checkRegionNone, {
      updateCheckboxGroupInput(session, inputId = "Reg", selected = NA)
    })
    
    ###### Floating Summary Table (Redundant Code) #####

    # output$countrySummary <- renderTable({
    #   tabeuc <- tableEuclidean()
    #   table("Country" = tabeuc$Country)
    #   })
    # 
    # output$regionSummary <- renderTable({
    #   tabeuc <- tableEuclidean()
    #   table("Region" = tabeuc$Region)
    #   })
    
    ##### Rendering Table #####
    # the table output of the closest database observations to the given observations.
    output$dis <- renderDataTable({
      req(datChanges(), tableEuclidean())
      
      dat <- datChanges()
      tabeuc <- tableEuclidean()
      
      # Validate that the table isn't empty
      validate(need(
        nrow(tabeuc) > 0,
        "No matching data found. Try adjusting the filters."
      ))
      
      # Define numeric columns for rounding
      numeric_cols <- intersect(
        c(
          "distance",
          "208/204Pb",
          "207/204Pb",
          "206Pb/204Pb",
          "204Pb/206Pb",
          "204Pb/207Pb",
          "204Pb/208Pb"
        ),
        colnames(tabeuc)
      )
      
      datatable(
        tabeuc,
        options = list(
          lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
          pageLength = 10,
          paging = TRUE,
          searching = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatRound(numeric_cols, digits = 6)
    })
    
    
    
    render_isotope_plot <- function(dat,
                                    obs,
                                    color_group,
                                    color_palette,
                                    y_labels) {
      # Generate color palette for groups (Country or Region)
      colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(dat[[color_group]])))
      
      # Helper function to create scatter plots
      create_plot <- function(y_column, obs_y) {
        plot_ly(
          data = dat,
          type = "scatter",
          x = ~ `206Pb/204Pb`,
          y = as.formula(paste0("~ `", y_column, "`")),
          mode = "markers",
          color = as.formula(paste0("~ `", color_group, "`")),
          hoverinfo = 'text',
          text = as.formula(paste0("~ `", color_group, "`")),
          colors = colors
        ) %>%
          add_trace(
            name = "New Obs",
            data = obs,
            x = ~ X3,
            y = as.formula(paste0("~ X", obs_y)),
            mode = "markers",
            marker = list(color = "black", size = 7)
          ) %>%
          layout(yaxis = list(title = y_column))
      }
      
      # Generate subplots
      subplot(
        create_plot(y_labels[1], 1),
        create_plot(y_labels[2], 2),
        nrows = 2,
        shareX = TRUE,
        titleX = TRUE
      )
    }
    
    # ---- 2D COUNTRY GRAPH ----
    output$obsGraph <- renderPlotly({
      dat <- datChanges()
      
      # Handle user input data
      if (input$Datatype == "0") {
        obs <- data.frame(
          X1 = input$x1,
          X2 = input$x2,
          X3 = input$x3,
          Country = "New Obs"
        )
      } else {
        clnm <- as.numeric(c(input$H1, input$H2, input$H3))
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        tab <- read.csv(inFile$datapath)
        names(tab) <- c("ID", "X1", "X2", "X3")
        
        obs <- data.frame(
          X1 = tab[, clnm[1]],
          X2 = tab[, clnm[2]],
          X3 = tab[, clnm[3]],
          Country = as.character(tab$ID)
        )
      }
      
      # Apply filtering
      mean206 <- mean(dat$`206Pb/204Pb`, na.rm = TRUE)
      sd206 <- sd(dat$`206Pb/204Pb`, na.rm = TRUE)
      dat <- dat[dat$`206Pb/204Pb` >= (mean206 - 3 * sd206) &
                   dat$`206Pb/204Pb` <= (mean206 + 3 * sd206) &
                   !is.na(dat$Country) &
                   dat$Country %in% input$selectCountry, ]
      
      # Call the reusable function
      render_isotope_plot(dat, obs, "Country", "Paired", c("208/204Pb", "207/204Pb"))
    })
    
    # ---- 2D REGION GRAPH ----
    output$regGraph <- renderPlotly({
      dat <- datChanges()
      
      # Handle user input data
      if (input$Datatype == "0") {
        obs <- data.frame(
          X1 = input$x1,
          X2 = input$x2,
          X3 = input$x3,
          Region = "New Obs"
        )
      } else {
        clnm <- as.numeric(c(input$H1, input$H2, input$H3))
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        tab <- read.csv(inFile$datapath)
        names(tab) <- c("ID", "X1", "X2", "X3")
        
        obs <- data.frame(
          X1 = tab[, clnm[1]],
          X2 = tab[, clnm[2]],
          X3 = tab[, clnm[3]],
          Region = as.character(tab$ID)
        )
      }
      
      # Apply filtering
      dat <- dat[dat$`206Pb/204Pb` < 20 &
                   !is.na(dat$Country) &
                   !is.na(dat$`208/204Pb`) &
                   !is.na(dat$`207/204Pb`) &
                   !is.na(dat$`206Pb/204Pb`) &
                   dat$Region %in% input$Reg, ]
      
      # Call the reusable function
      render_isotope_plot(dat, obs, "Region", "Paired", c("208/204Pb", "207/204Pb"))
    })
    
    
    ##### 3D Country Plot ######
    
    render_3d_isotope_plot <- function(dat,
                                       obs,
                                       filter_column,
                                       filter_values,
                                       color_column) {
      # Apply standard deviation filtering on `206Pb/204Pb`
      mean206 <- mean(dat$`206Pb/204Pb`, na.rm = TRUE)
      sd206 <- sd(dat$`206Pb/204Pb`, na.rm = TRUE)
      
      filtered_data <- dat[dat$`206Pb/204Pb` >= (mean206 - 3 * sd206) &
                             dat$`206Pb/204Pb` <= (mean206 + 3 * sd206) &
                             !is.na(dat[[filter_column]]) &
                             !is.na(dat$`208/204Pb`) &
                             !is.na(dat$`207/204Pb`) &
                             !is.na(dat$`206Pb/204Pb`) &
                             dat[[filter_column]] %in% filter_values, ]
      
      # Generate color palette
      colors <- colorRampPalette(brewer.pal(9, "Set3"))(length(unique(filtered_data[[color_column]])))
      
      # Create the 3D scatter plot
      plt3 <- plot_ly(
        filtered_data,
        x = ~ `206Pb/204Pb`,
        y = ~ `208/204Pb`,
        z = ~ `207/204Pb`,
        type = "scatter3d",
        text = ~ paste("ID: ", `ID in database`),
        mode = "markers",
        color = as.formula(paste0("~ `", color_column, "`")),
        marker = list(size = 4),
        colors = colors,
        width = 1000,
        height = 1000
      ) %>%
        layout(
          autosize = FALSE,
          margin = list(
            l = 20,
            r = 20,
            b = 20,
            t = 20,
            pad = 10
          ),
          legend = list(font = list(size = 14), legendgroup = color_column)
        )
      
      # Add new observations
      plt3 <- add_markers(
        p = plt3,
        x = obs[, 4],
        y = obs[, 2],
        z = obs[, 3],
        type = "scatter3d",
        text = ~ paste(obs$ID),
        marker = list(color = "black", size = 5)
      )
      
      return(plt3)
    }
    
    # ---- OBSERVED GRAPH (3D) ----
    output$obsGraph3d <- renderPlotly({
      dat <- datChanges()
      
      # Handle user input data
      if (input$Datatype == "0") {
        obs <- data.frame(matrix(t(c(
          input$x1, input$x2, input$x3
        )), ncol = 3, byrow = FALSE))
        obs <- cbind("ID" = "New Obs", obs)
      } else {
        clnm <- as.numeric(c(input$H1, input$H2, input$H3))
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        tab <- read.csv(inFile$datapath)
        names(tab) <- c("ID", "X1", "X2", "X3")
        
        obs <- data.frame(matrix(
          cbind(
            "208/204" = tab[, clnm[1]],
            "207/204" = tab[, clnm[2]],
            "206/204" = tab[, clnm[3]]
          ),
          ncol = 3,
          byrow = FALSE
        ))
        
        obs <- cbind("ID" = as.character.factor(tab$ID), obs)
      }
      
      render_3d_isotope_plot(dat, obs, "Country", input$selectCountry, "Country")
    })
    
    # ---- REGIONAL GRAPH (3D) ----
    output$regGraph3d <- renderPlotly({
      dat <- datChanges()
      
      # Handle user input data
      if (input$Datatype == "0") {
        obs <- data.frame(matrix(t(c(
          input$x1, input$x2, input$x3
        )), ncol = 3, byrow = FALSE))
        obs <- cbind("ID" = "New Obs", obs)
      } else {
        clnm <- as.numeric(c(input$H1, input$H2, input$H3))
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        
        tab <- read.csv(inFile$datapath)
        names(tab) <- c("ID", "X1", "X2", "X3")
        
        obs <- data.frame(matrix(
          cbind(
            "208/204" = tab[, clnm[1]],
            "207/204" = tab[, clnm[2]],
            "206/204" = tab[, clnm[3]]
          ),
          ncol = 3,
          byrow = FALSE
        ))
        
        obs <- cbind("ID" = as.character.factor(tab$ID), obs)
      }
      
      render_3d_isotope_plot(dat, obs, "Region", input$Reg, "Region")
      })

      ###### CSV Download Handeler ######
      output$download <- downloadHandler(
        filename = "LIA Database Table.csv",
        content = function(file) {
          write.csv(x = tableEuclidean(), file)
        }
      )
    })