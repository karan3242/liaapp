# global

##### Packages #####

# List of packages needed
packages <- c("tidyverse",
              "readxl",
              "plotly",
              "shiny",
              "RColorBrewer",
              "markdown",
              "DT")

# Install uninstalled Packages and Load the libraries.
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

##### Loading LIA Database #####

# Path to data (Leave this at top level)
path <- "database.xlsx"

dat <- read_excel(path = path, col_types = c(rep("guess", 22), 
                                             "text", 
                                             rep("guess", 5)
                                             )
                  )

colnames(dat) <- c("Source of data",
                   "Sample Number",
                   "Country","Top Region","Region", "Sub Region",
                   "Deposit/ Province", "Mine, Ore deposit or sampling spot",
                   "Collected by", "Type", "Type for DB",
                   "Main constituent",	"Main constituent for DB",
                   "Description/ Mineral", 
                   "206Pb/204Pb", "208/204Pb", "207/204Pb", 
                   "Date analysed",
                   "ref. #", "reference", "year", "ID in database", "comments",
                   "204Pb/206Pb",	"204Pb/208Pb", "204Pb/207Pb", 
                   "SuspectedError", "medRegion")

# Lower case the Type of DB
dat$`Type for DB` <- tolower(dat$`Type for DB`)

# Lower case 'Main Constitute of DB
dat$`Main constituent for DB` <- tolower(dat$`Main constituent for DB`) 

# Suspected Error as labels
dat$SuspectedError[dat$SuspectedError == 0] <- "Suspected outlier"
dat$SuspectedError[dat$SuspectedError == 0.5] <- "Not enough data"
dat$SuspectedError[dat$SuspectedError == 1] <- "OK"

##### Lists of User Options in ui.R #####

# Make list of Main Elements
main <- sapply(unique(dat$`Main constituent for DB`), list)
names(main) <- ifelse(is.na(names(main)) | tolower(names(main)) == "na", 
                      "Not Available", 
                      names(main))

if ("Not Available" %in% names(main)) {
  main[["Not Available"]] <- unique(unlist(main[names(main) == "Not Available"]))
  main <- main[!duplicated(names(main))]  # Remove duplicates
}

# Make list of Source types
type <- sapply(unique(dat$`Type for DB`), list)
names(type) <- ifelse(is.na(names(type)) | tolower(names(type)) == "na", 
                      "Not Available", 
                      names(type))

if ("Not Available" %in% names(type)) {
  type[["Not Available"]] <- unique(unlist(type[names(type) == "Not Available"]))
  type <- type[!duplicated(names(type))]  # Remove duplicates
}

# Make a list out Suspected Errors
outlier <- sapply(unique(dat$SuspectedError), list)

# Make a list of Countries
CountriesVar <- sapply(unique(dat$Country), list)

# Make a list of Regions
RegionsVar <- sapply(unique(dat$Region), list)

##### Data base function #####

data.base.function <- function(dat) {
  
  # Internal function: Mahalanobis distance-based outlier detection
  sign1corrected <- function (x, makeplot = FALSE, qcrit = 0.975, ...) {
    p <- ncol(x)
    n <- nrow(x)
    x.mad <- apply(x, 2, mad)
    if (any(x.mad == 0))
      stop("More than 50% equal values in one or more variables!")
    x.sc <- scale(x, apply(x, 2, median), x.mad)
    
    if (any(row_medians_match == ncol(x))) {
      matching_rows <- which(row_medians_match == ncol(x))
      x.sc[matching_rows, 1] <- x.sc[matching_rows, 1] + 1e-7  # Slight perturbation
    }
    
    xs <- x.sc / sqrt(apply(x.sc ^ 2, 1, sum))
    xs.evec <- svd(xs)$v
    xs.pc <- x.sc %*% xs.evec
    xs.pcscal <- apply(xs.pc, 2, mad) ^ 2
    xs.pcorder <- order(xs.pcscal, decreasing = TRUE)
    
    p1 <- min(p - 1, n - 1)
    covm1 <- xs.evec[, xs.pcorder[1:p1]] %*% diag(1 / xs.pcscal[xs.pcorder[1:p1]]) %*%
      t(xs.evec[, xs.pcorder[1:p1]])
    x.dist <- sqrt(mahalanobis(x.sc, rep(0, p), covm1, inverted = TRUE))
    
    const <- sqrt(qchisq(qcrit, p1))
    wfinal01 <- (x.dist < const) * 1
    
    if (makeplot) {
      op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
      on.exit(par(op))
      plot(x.dist, xlab = "Index", ylab = "Distance", ...)
      abline(h = const)
      plot(
        wfinal01,
        xlab = "Index",
        ylab = "Final 0/1 weight",
        ylim = c(0, 1),
        ...
      )
    }
    
    list(wfinal01 = wfinal01,
         x.dist = x.dist,
         const = const)
  }
  
  # Euclidean distance function
  euc <- function(x, y) {
    sqrt(sum((x - y) ^ 2))
  }
  
  # Adding calculated columns
  ourdat <- dat %>%
    filter(!is.na(`206Pb/204Pb`), 
           !is.na(`208/204 pb`), 
           !is.na(`207/204 pb`), 
           !is.na(Country)) %>%
    mutate(
      `ID in database` = row_number(),
      `204Pb/206Pb` = 1 / `206Pb/204Pb`,
      `204Pb/208Pb` = 1 / `208/204 pb`,
      `204Pb/207Pb` = 1 / `207/204 pb`
    )

  ourdat$`Date analysed` <- as.character(as.Date(ourdat$`Date analysed`))
  
  # Identify regions with more than 10 samples
  region <- names(table(ourdat$Region)[table(ourdat$Region) > 10])
  ourdat$Region[is.na(ourdat$Region)] <- "No Region"
  
  # Initialize error vectors
  error <- rep(NA, nrow(ourdat))
  medRegion <- as.vector(matrix(nrow = nrow(ourdat)))
  
  for(reg in region){
    
    tab <- as.matrix(ourdat[ourdat$Region == reg,c("206Pb/204Pb","208/204Pb","207/204Pb")])
    error[ourdat$Region == reg] <- sign1corrected(x = tab, makeplot = FALSE)$wfinal01
    
    medRegion[ourdat$Region == reg] <- apply(X=tab,MARGIN = 1,FUN = euc,x=apply(tab,2,median))
    
  }
  
  error[is.na(error)] <- 0.5
  # error vector:  1 - Not suspected with error, 
  #              0.5 - not enough data to make an informed decision if error.
  #                0 - suspected in error. 
  medRegion[is.na(medRegion)] <- -1
  ourdat$SuspectedError <- error
  ourdat$medRegion <- medRegion 
  
  return(ourdat)
}