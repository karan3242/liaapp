#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # the updating of the base file.
  update_function <- reactive({
    if (is.null(input$base_file)) {
      new_dat <- NULL
    } else {
      update_path <- input$base_file
      
      file.rename(update_path$datapath,
                  paste(update_path$datapath, ".xlsx", sep = ""))
      
      new_dat <- suppressWarnings(read_excel(
        path = paste(update_path$datapath, ".xlsx", sep = ""),
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
      ))
      
      # the updating function on the new file we were given.
      new_dat <- data.base.function(new_dat)
      
    }
    
    new_dat
    
  })
  
  output$download_file <- renderUI({
    if (!is.null(update_function()))
      downloadButton(outputId = "download_new_dat", label = "Download")
  })
  
  output$download_new_dat <- downloadHandler(
    filename = "database.xlsx",
    content = function(file) {
      xlsx::write.xlsx(
        file = file,
        x = data.frame(update_function()),
        sheetName = "data",
        row.names = FALSE,
        showNA = FALSE
      )
    }
    
  )
  
  
  
  
  
  # function to find wuclidean distance in a 3d graph.
  euc <- function(x, y) {
    sqrt(sum((x - y) ^ 2))
    
  }
  
  # Added filtering the data.
  datChanges <- reactive({
    errorFilter <- input$errorFilter
    
    ConstFilter <- input$ConstFilter
    
    typeFilter <- input$typeFilter
    
    ind <- c(dat$SuspectedError %in% errorFilter) &
      dat$`Main constituent for DB` %in% ConstFilter &
      dat$`Type for DB` %in% typeFilter
    
    datChanges <- dat[ind, ]
    
    datChanges
    
    
  })
  
  tableEuclidean <- reactive({
    dat <- datChanges()
    
    if (input$Datatype == "0") {
      # the given observations.
      obs <- c(input$x1, input$x2, input$x3)
      # the database observations.
      datobs <- cbind(dat$`208/204Pb`, dat$`207/204Pb`, dat$`206Pb/204Pb`)
      # run "euc" on the table with a refrence to the given observation, and give the 5 closest observations.
      distance <- apply(
        X = datobs,
        MARGIN = 1,
        FUN = euc,
        x = obs
      )
      place <- order(distance, decreasing = FALSE)[1:input$numobs]
      
      tabeuc <- cbind(distance, dat)[place, ]
      
      # print out the following data on the database observations.
      tabeuc
      
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3)) # in case the columns are given in the wrong order.
      # the file - a .csv file with 4 columns.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      # the right order of the table.
      tab1 <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      ids <- as.character(tab[, 1])
      # this function copies the process of asingle observation.
      eucTable <- function(vec) {
        datobs <- cbind(dat$`208/204Pb`, dat$`207/204Pb`, dat$`206Pb/204Pb`)
        
        distance <- apply(
          X = datobs,
          MARGIN = 1,
          FUN = euc,
          x = vec
        )
        
        place <- order(distance, decreasing = FALSE)[1:input$numobs]
        
        return(cbind(distance, dat)[place, ])
        
      }
      # now the process is repeated on every given observation.
      tabeuc <- NULL
      
      for (i in 1:nrow(tab1)) {
        obs <- tab1[i, ]
        
        tmp <- cbind("ID" = rep(ids[i], input$numobs), eucTable(obs))
        
        tabeuc <- rbind(tabeuc, tmp)
      }
      
      tabeuc
    }
    
  })
  # the code that allows us to update the country and region filters.
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
  
  # the summary tables
  output$countrySummary <- renderTable({
    tabeuc <- tableEuclidean()
    table("Country" = tabeuc$Country)
    
    
  })
  
  output$regionSummary <- renderTable({
    tabeuc <- tableEuclidean()
    table("Region" = tabeuc$Region)
    
  })
  
  # the table output of the closest database observations to the given observations.
  output$dis <- renderDataTable({
    dat <- datChanges()
    # if one observation is given.
    tabeuc <- tableEuclidean()
    
    datatable(
      tabeuc,
      options = list(
        lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
        pageLength = 15,
        paging = TRUE,
        searching = TRUE
      ),
      rownames = rep("", times = nrow(tabeuc))
    ) %>%
      formatRound(
        c(
          "distance",
          "208/204Pb",
          "207/204Pb",
          "206Pb/204Pb",
          "204Pb/206Pb",
          "204Pb/207Pb",
          "204Pb/208Pb"
        ),
        digits = 6
      )
    
  })
  
  # the first 2d graph.
  output$obsGraph1 <- renderPlotly({
    dat <- datChanges()
    # the observations. (both single and table)
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Country <- "New Obs"
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Country <- as.character.factor(tab$ID)
    }
    
    CountryFilter <- input$selectCountry
    
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Country %in% CountryFilter
    dat <- dat[ind, ]
    
    plt1 <- plot_ly(
      data = dat,
      type = "scatter",
      x = ~ `206Pb/204Pb`,
      y = ~ `208/204Pb`,
      mode = "markers",
      color = ~ Country,
      hoverinfo = 'text',
      text = ~ Country
    )
    
    plt1 <- add_trace(
      p = plt1,
      name = "New Obs",
      data = obs,
      x = ~ X3,
      y = ~ X1,
      mode = "markers",
      color = "black",
      marker = list(color = "black", size = 5)
    )
    
    plt1
  })
  
  # the second 2d graph.
  output$obsGraph2 <- renderPlotly({
    dat <- datChanges()
    # the same process as the 1st 2d graph.
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Country <- "New Obs"
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Country <- as.character.factor(tab$ID)
    }
    
    CountryFilter <- input$selectCountry
    
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Country %in% CountryFilter
    dat <- dat[ind, ]
    
    plt1 <- plot_ly(
      data = dat,
      type = "scatter",
      x = ~ `206Pb/204Pb`,
      y = ~ `207/204Pb`,
      mode = "markers",
      color = ~ Country,
      hoverinfo = 'text',
      text = ~ Country
    )
    
    plt1 <- add_trace(
      p = plt1,
      name = "New Obs",
      data = obs,
      x = ~ X3,
      y = ~ X2,
      mode = "markers",
      color = "black",
      marker = list(color = "black", size = 5)
    )
    
    plt1
  })
  
  # a 3d graph using "plotly".
  output$obsGraph3d <- renderPlotly({
    dat <- datChanges()
    # the observations. (both single and table)
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs <- cbind("ID" = "New Obs", obs)
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs <- cbind("ID" = as.character.factor(tab$ID), obs)
    }
    
    CountryFilter <- input$selectCountry
    
    
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Country %in% CountryFilter
    
    d <- dat[ind, ]
    
    plt3 <- plot_ly(
      d,
      x = d$`206Pb/204Pb`,
      y = d$`208/204Pb`,
      z = d$`207/204Pb`,
      type = "scatter3d",
      text = paste("ID: ", d$`ID in database`),
      mode = "markers",
      color = d$Country,
      marker = list(size = 4),
      colors = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(d$Country))),
      width = 1000,
      height = 1000
    ) %>%
      layout(
        autosize = F,
        margin = list(
          l = 20,
          r = 20,
          b = 20,
          t = 20,
          pad = 10
        ),
        legend = list(
          x = 0,
          y = 1,
          font = list(size = 14),
          legendgroup = "Country",
          orientation = "h"
        )
      )  # Added - marker's place and direction
    
    plt3 <- add_markers(
      p = plt3,
      x = obs[, 4],
      y = obs[, 2],
      z = obs[, 3],
      type = "scatter3d",
      text = paste(obs$ID),
      color = "New obs",
      marker = list(color = "black", size = 5)
    )
    
    
    plt3
    
    
  })
  
  output$regGraph1 <- renderPlotly({
    dat <- datChanges()
    # the observations. (both single and table)
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Region <- "New Obs"
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Region <- as.character.factor(tab$ID)
    }
    
    # CountryFilter <- input$selectCountry
    RegionFilter <- input$Reg
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Region %in% RegionFilter
    dat <- dat[ind, ]
    
    # plt1 <- ggplot(dat,aes(x = dat$`206Pb/204Pb`,y = dat$`208/204Pb`,colour = dat$Region)) +
    #   geom_point(aes(shape = factor(dat$Region)))+ # the database plot.
    #   geom_point(data = obs, x = obs[,3],y = obs[,1],colour = list("New obs" = "black"),size = 2) +  #the new observation/s are added
    #   ggtitle("Isotope Ratio 206 by 208") + # Name the graph.
    #   xlab("206Pb/204Pb") + ylab("208Pb/204Pb") + # name the axis.
    #   guides(colour=guide_legend(title="Region")) + # the legend's title
    #   theme(plot.title = element_text(lineheight=5, face="bold",size = 12),
    #         legend.text = element_text(size = 11),
    #         legend.title = element_text(size = 12 ,face = "bold"),
    #         axis.title = element_text(size = rel(1))) +
    #   scale_shape_manual(values = 0:length(RegionFilter),name = "Region")
    #
    # ggplotly(plt1) %>% # giving the plot as plotly, more interactive and pleasing to the eye...
    #   layout(legend = list(font = list(size = 14)))
    #
    plt1 <- plot_ly(
      data = dat,
      type = "scatter",
      x = ~ `206Pb/204Pb`,
      y = ~ `208/204Pb`,
      mode = "markers",
      color = ~ Region,
      hoverinfo = 'text',
      text = ~ Region
    )
    
    plt1 <- add_trace(
      p = plt1,
      name = "New Obs",
      data = obs,
      x = ~ X3,
      y = ~ X1,
      mode = "markers",
      color = "black",
      marker = list(color = "black", size = 5)
    )
    
    plt1
  })
  
  # the second 2d graph.
  output$regGraph2 <- renderPlotly({
    dat <- datChanges()
    # the observations. (both single and table)
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Region <- "New Obs"
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      # a little adjusting.
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs$Region <- as.character.factor(tab$ID)
    }
    
    # CountryFilter <- input$selectCountry
    RegionFilter <- input$Reg
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Region %in% RegionFilter
    dat <- dat[ind, ]
    
    # plt2 <- ggplot(dat,aes(x = dat$`206Pb/204Pb`,y = dat$`207/204Pb`,colour = dat$Region)) +
    #   geom_point(aes(shape = factor(dat$Region)))+ # the database plot.
    #   geom_point(data = obs, x = obs[,3],y = obs[,2],colour = list("New obs" = "black"),size = 2) +  #the new observation/s are added
    #   ggtitle("Isotope Ratio 206 by 207") + # Name the graph.
    #   xlab("206Pb/204Pb") + ylab("207Pb/204Pb") + # name the axis.
    #   guides(colour=guide_legend(title="Region")) + # the legend's title
    #   theme(plot.title = element_text(lineheight=5, face="bold",size = 12),
    #         legend.text = element_text(size = 11),
    #         legend.title = element_text(size = 12 ,face = "bold"),
    #         axis.title = element_text(size = rel(1))) +
    #   scale_shape_manual(values = 0:length(RegionFilter), name = "Region")
    #
    # ggplotly(plt2) %>% # giving the plot as plotly, more interactive and pleasing to the eye...
    #   layout(legend = list(font = list(size = 14)))
    
    plt1 <- plot_ly(
      data = dat,
      type = "scatter",
      x = ~ `206Pb/204Pb`,
      y = ~ `207/204Pb`,
      mode = "markers",
      color = ~ Region,
      hoverinfo = 'text',
      text = ~ Region
    )
    
    plt1 <- add_trace(
      p = plt1,
      name = "New Obs",
      data = obs,
      x = ~ X3,
      y = ~ X2,
      mode = "markers",
      color = "black",
      marker = list(color = "black", size = 5)
    )
    
    plt1
    
  })
  
  # a 3d graph using "plotly".
  output$regGraph3d <- renderPlotly({
    dat <- datChanges()
    # the observations. (both single and table)
    if (input$Datatype == "0") {
      obs <- t(c(input$x1, input$x2, input$x3))
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs <- cbind("ID" = "New Obs", obs)
    } else {
      clnm <- as.numeric(c(input$H1, input$H2, input$H3))
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tab <- read.csv(inFile$datapath)
      names(tab) <- c("ID", "X1", "X2", "X3")
      obs <- cbind(
        "208/204" = tab[, clnm[1]],
        "207/204" = tab[, clnm[2]],
        "206/204" = tab[, clnm[3]]
      )
      
      obs <- data.frame(matrix(obs, ncol = 3, byrow = FALSE))
      obs <- cbind("ID" = as.character.factor(tab$ID), obs)
    }
    
    RegionFilter <- input$Reg
    
    
    # NEED TO GO AWAY, WRONG PRCESS NEED TO BE REPLACED WITH SD.
    ind <- dat$`206Pb/204Pb` < 20 &
      !is.na(dat$Country) &
      !is.na(dat$`208/204Pb`) &
      !is.na(dat$`207/204Pb`) &
      !is.na(dat$`206Pb/204Pb`) & dat$Region %in% RegionFilter
    
    d <- dat[ind, ]
    
    plt3 <- plot_ly(
      d,
      x = d$`206Pb/204Pb`,
      y = d$`208/204Pb`,
      z = d$`207/204Pb`,
      type = "scatter3d",
      text = paste("ID: ", d$`ID in database`),
      mode = "markers",
      color = d$Region,
      marker = list(size = 4),
      colors = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(d$Region))),
      width = 1000,
      height = 1000
    ) %>%
      layout(
        autosize = F,
        margin = list(
          l = 20,
          r = 20,
          b = 20,
          t = 20,
          pad = 10
        ),
        legend = list(font = list(size = 14), legendgroup = "Region")
      )  # Added - marker's place and direction
    
    plt3 <- add_markers(
      p = plt3,
      x = obs[, 4],
      y = obs[, 2],
      z = obs[, 3],
      type = "scatter3d",
      text = paste(obs$ID),
      color = "New obs",
      marker = list(color = "black", size = 5)
    )
    
    
    plt3
    
    
  })
  
  output$download <- downloadHandler(
    filename = "LIA Database Table.csv",
    content = function(file) {
      write.csv(x = tableEuclidean(), file)
    }
    
    
  )
})