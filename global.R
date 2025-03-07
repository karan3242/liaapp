
# global
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("shiny")
#install.packages("xlsx")
#install.packages("markdown")
#install.packages("DT")

library("readxl")
library("ggplot2")
library("plotly")
library("shiny")
#library("xlsx")
library("RColorBrewer")
library("markdown")
library("DT")

# changed the database from "a" to "dat" (need to fix that ANNOYING 1st column)
path <- "C:\\Users\\User\\Dropbox\\LIA database\\App\\LIA database\\app 2019\\database.xlsx"

dat <- suppressWarnings(read_excel(path = path))
# these are all problems we need to address. I took them out because they annoy me.
# dat <- dat[,-1] # number rows - redundant.

colnames(dat) <- c("Source of data",
                   "Sample Number",
                   "Country","Top Region","Region", "Sub Region",
                   "Deposit/ Province", "Mine, Ore deposit or sampling spot",
                   "Collected by", "Type", "Type for DB",
                   "Main constituent",	"Main constituent for DB",
                   "Description/ Mineral", "206Pb/204Pb", "208/204 pb", "207/204 pb", "Date analysed",
                   "ref. #", "reference", "year", "ID in database", "comments",
                   "204Pb/206Pb",	"204Pb/208Pb", "204Pb/207Pb", "SuspectedError" ,"medRegion")

dat$`Type for DB` <- tolower(dat$`Type for DB`)
dat$`Main constituent for DB` <- tolower(dat$`Main constituent for DB`)

dat$SuspectedError[dat$SuspectedError==0]<-"Suspected outlier"
dat$SuspectedError[dat$SuspectedError==0.5]<-"Not enough data"
dat$SuspectedError[dat$SuspectedError==1]<- "OK"


main <- unique(dat$`Main constituent for DB`)
main <- sapply(main,list)
names(main)[names(main) == "na"] <- "Not Available"


type <- unique(dat$`Type for DB`)
type <- sapply(type,list)
names(type)[names(type) == "na"] <- "Not Available"


outlier <- unique(dat$SuspectedError)
outlier <- sapply(outlier,list)

CountriesVar <- sapply(unique(dat$Country),list)
RegionsVar <- sapply(unique(dat$Region),list)
9
data.base.function <- function(dat){
  sign1corrected <- function (x, makeplot = FALSE, qcrit = 0.975, ...){
    p = ncol(x)
    n = nrow(x)
    x.mad = apply(x, 2, mad)
    if (any(x.mad == 0)) 
      stop("More than 50% equal values in one or more variables!")
    x.sc <- scale(x, apply(x, 2, median), x.mad)
    med <- apply(x,2,median) # my addition (plus the condition...)
    if(sum(apply(x,1,function(x){sum(x == med)})==3) > 0){x.sc[which(apply(x,1,function(x){sum(x == med)})==3),1] <- x.sc[which(apply(x,1,function(x){sum(x == med)})==3),1]+0.0000001}
    xs <- x.sc/sqrt(apply(x.sc^2, 1, sum))
    xs.evec <- svd(xs)$v
    xs.pc <- x.sc %*% xs.evec
    xs.pcscal <- apply(xs.pc, 2, mad)^2
    xs.pcorder <- order(xs.pcscal, decreasing = TRUE)
    p1 = min(p - 1, n - 1)
    covm1 = xs.evec[, xs.pcorder[1:p1]] %*% diag(1/xs.pcscal[xs.pcorder[1:p1]]) %*% 
      t(xs.evec[, xs.pcorder[1:p1]])
    x.dist = sqrt(mahalanobis(x.sc, rep(0, p), covm1, inverted = TRUE))
    const <- sqrt(qchisq(qcrit, p1))
    wfinal01 <- (x.dist < const) * 1
    if (makeplot) {
      op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
      on.exit(par(op))
      plot(x.dist, xlab = "Index", ylab = "Distance", ...)
      abline(h = const)
      plot(wfinal01, xlab = "Index", ylab = "Final 0/1 weight", 
           ylim = c(0, 1), ...)
    }
    list(wfinal01 = wfinal01, x.dist = x.dist, const = const)
  }
  # the euclidean function.
  euc<- function(x,y){
    
    sqrt(sum((x-y)^2))
    
  }
  # a basic demand for the function to worl prperly is the existence of the isotopes, and a country.
  ourdat <- dat[!is.na(dat$`206Pb/204Pb`) & !is.na(dat$`208/204 pb`) & !is.na(dat$`207/204 pb`) & !is.na(dat$Country),]
  
  # a fairly straightforward additions to the database.
  ourdat$`ID in database` <- 1:nrow(ourdat)
  ourdat$'204Pb/206Pb' <- 1/ourdat$`206Pb/204Pb`
  ourdat$'204Pb/208Pb' <- 1/ourdat$`208/204 pb`
  ourdat$'204Pb/207Pb' <- 1/ourdat$`207/204 pb`
  # an annoying little bug, when reading the date, it was read to complicated (dd-mm-yyyy hh:mm:ss) I simplified it.
  ourdat$`Date analysed` <- as.character(as.Date.character(ourdat$`Date analysed`))
  
  # the solution using malhalonis distance with the mvoutlier package by region
  # another demand is that for outliers we need a bit of data, minimum 10 observation.
  region <- names(table(ourdat$Region)[table(ourdat$Region) > 10]) 
  # those without a region will get another value (to avoid NA related problems.)
  ourdat$Region[is.na(ourdat$Region)] <- "No Region"
  # the forming of the error vector.
  error <- as.vector(matrix(nrow = nrow(ourdat)))
  medRegion <- as.vector(matrix(nrow = nrow(ourdat)))
  for(reg in region){
    
    tab <- as.matrix(ourdat[ourdat$Region == reg,c("206Pb/204Pb","208/204 pb","207/204 pb")])
    # print(ourdat[ourdat$Region == reg,c("Region","Country","Sample Number")])
    error[ourdat$Region == reg] <- sign1corrected(x = tab, makeplot = FALSE)$wfinal01
    
    medRegion[ourdat$Region == reg] <- apply(X=tab,MARGIN = 1,FUN = euc,x=apply(tab,2,median))
    
  }
  
  error[is.na(error)] <- 0.5
  # error vector: 1 - Not suspected with error, 
  #             0.5 - not enough data to make an informed desicion if error.
  #               0 - suspected in error. 
  medRegion[is.na(medRegion)] <- -1
  ourdat$SuspectedError <- error # a check i ran by hand is "verifying" the results.
  ourdat$medRegion <- medRegion 
  
  # write the data to a new file (eventually this process will be places ONLY with the original data so this stage is redundant in the long run.)
  #xlsx::write.xlsx(ourdat, file = "C:\\Users\\shirk\\Documents\\Archeology app\\LIA database\\database.xlsx", col.names = TRUE,showNA = FALSE) # from package xlsx
  
  return(ourdat)
}


# a link to solve the colours problem is :
# http://novyden.blogspot.co.il/2013/09/how-to-expand-color-palette-with-ggplot.html

