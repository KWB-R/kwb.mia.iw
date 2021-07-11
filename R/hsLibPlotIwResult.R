# hsNameFilter -----------------------------------------------------------------

#' Name Filter
#' 
#' Include / exclude elements from vector by patterns
#' 
#' @param x vector of character
#' @param posPtrn pattern matching elements to be included
#' @param negPtrn pattern matching elements to be excluded
#' @return elements of \emph{x} matching \emph{posPtrn} and not matching \emph{negPtrn}
#' @export
hsNameFilter <- function(x, posPtrn = NULL, negPtrn = NULL) 
{
  if (! is.null(posPtrn)) {
    for (pPtrn in posPtrn)
      x <- grep(pPtrn, x, value = TRUE)
  }
  if (! is.null(negPtrn)) {
    for (nPtrn in negPtrn)
      x <- grep(nPtrn, x, value = TRUE, invert = TRUE)
  }
  x
}

# hsTimeBarPlot ----------------------------------------------------------------

#' Plot Type 1
#' 
#' Plot type 1: boxes with mean flow as \code{height} and event duration as
#' width
#'
#' @param t1 begin times of bars
#' @param t2 end times of bars
#' @param height bar heights
#' @param tlim limits of time axis. Default: c(min(\code{t1}), max(\code{t2}))
#' @param ylim limits of y axis, Default: c(min(\code{height}),
#'   max(\code{height}))
#' @param ylog if TRUE, y axis is scaled logarithmically, else linearly
#' @param ymult numbers to be multiplied by exponents of ten for labelling of y
#'   axis when \emph{ylog} is TRUE
#' @param main plot title
#' @param tlab label of time axis
#' @param ylab label of y axis
#' @param time.format format of time axis labels
#' @param tlab.mindist minimum "time distance" between time labels, in seconds
#' @param col bar colour
#' @param cex.all general expansion factor to be applied to \code{cex.legend},
#'   \code{cex.text}, \code{cex.axis}, if not given
#' @param cex.legend expansion factor for legend texts
#' @param cex.text expansion factor of texts within plot (bar numbers)
#' @param cex.axis expansion factor of axis labels
#' @param legend.values values to be shown in legend, default: \code{height}
#'   values
#' @param legend.sort if TRUE, legend is sorted decreasingly by
#'   \code{legend.values}
#' @param legend.format format string to be used by sprintf for generation of
#'   legend entries from legend values, e.g. "V = \%8.0f m3"
#' @param legend.title legend title
#' @param mar plot margins
#' @export
hsTimeBarPlot <- function(
  t1, 
  t2,
  height,
  tlim = c(min(t1), max(t2)), 
  ylim = NULL, # c(min(height), max(height)), 
  ylog = TRUE,
  ymult = c(1, 2, 3, 5),
  main = "hsTimeBarPlot",
  tlab = "time",
  ylab = "height",  
  time.format = .defaultDateFormat(),
  tlab.mindist = 86400,
  col = "grey",
  cex.all = 0.8,
  cex.legend = cex.all,
  cex.text = cex.all,
  cex.axis = cex.all,
  legend.values = height,
  legend.sort = TRUE,
  legend.format = .defaultLegendFormat(),
  legend.title = "Legend:",
  mar = c(6,5,5,10)
) 
{
  # Set plot area with wide right margin for legend
  # family = mono: "true type" font
  oldpar <- graphics::par(mar = mar)
  
  # empty plot
  graphics::plot(t1, height, type = "n", 
       xlim = tlim,  ylim = ylim, log = ifelse(ylog, "y", ""), 
       xaxt = "n", #axes = FALSE, 
       main = main, xlab = "", ylab = "", las = 1)
  
  # Label axes manually
  graphics::mtext(tlab, side = 1, line = 4)
  graphics::mtext(ylab, side = 2, line = 4)
  
  # add a grid on the y axis
  if (FALSE) {
    if (ylog) {
      eylim <- log(ylim, base = 10)  
      eylim <- c(floor(eylim[1]), ceiling(eylim[2]))
      epos <- 10^(eylim[1]:eylim[2])
      ypos <- NULL
      for (mult in ymult) {
        ypos <- sort(c(ypos, mult * epos))
      }    
    } else {
      ypos <- graphics::axTicks(2, log = FALSE)
    }
    
    graphics::abline(h = ypos, lty = 2, col = "grey")    
  }
  
  # Get y coordinate of bottom
  y0 <- ifelse(is.null(ylim), graphics::par("usr")[3], ylim[1])
  #if (ylog) {
  #  y0 <- 10^y0
  #}
  #cat("y0 = ", y0, "\n")
  
  # Draw rectangles
  graphics::rect(t1, y0, t2, height, col = col)
  
  # Show x axis with days of event begin as labels
  labs <- kwb.plot::niceLabels(
    label = format.Date(t1, time.format), 
    labelpos = as.integer(t1),
    mindist = tlab.mindist
  )
  
  # Plot x axis
  graphics::axis(1, at = t1, labels = labs, las = 3, cex.axis = cex.axis, pos = y0)
  
  # Plot y axis
  #graphics::axis(2) # , at = ypos, labels = ypos, las = 1, cex.axis = cex.axis)
  
  # Plot number on top of each rectangle
  labpos <- as.integer(t1)/2 + as.integer(t2)/2
  labs <- kwb.plot::niceLabels(
    label = 1:length(t1), 
    labelpos = labpos,
    mindist = tlab.mindist
  )
  
  # y offset used to shift labels in positive y direction
  yoff <- 0.03 * if (ylog) {
    height
  } else {
    diff(range(graphics::axTicks(2)))
  }
  
  graphics::text(labpos, height + yoff, labels = labs, cex = cex.text)
  
  # Plot a legend if legend values are given
  if (! is.null(legend.values)) {
    
    # prepare legend texts
    ltexts <- sprintf(paste("%3d:", legend.format), 1:length(t1), legend.values)
    
    # if required, sort legend texts by volume, decreasingly
    if (legend.sort) {
      ltexts <- ltexts[order(legend.values, decreasing = TRUE)]
    }
    
    # add total value
    ltexts <- c(ltexts, sprintf(paste("tot:", legend.format), sum(legend.values)))
    
    # Save currently used font family
    ffamily <- graphics::par("family")
    
    # temporarily set true-type font for the legend    
    graphics::par(family = "mono")
    
    graphics::legend(
      "right", ltexts, inset = -0.2, xpd = TRUE, cex = cex.legend,
      title = legend.title
    )
    
    # Reset font
    graphics::par(family = ffamily)
  }
  
  # Reset graphical parameters
  graphics::par(oldpar)  
}

# .defaultDateFormat -----------------------------------------------------------
.defaultDateFormat <- function()
{
  "%Y-%m-%d"
}

# .defaultLegendFormat ---------------------------------------------------------
.defaultLegendFormat <- function()
{
  "height: %f"
}

# hsGetIwResult ----------------------------------------------------------------

#' Get Infoworks Result
#' 
#' @param file \code{file}
#' @param columns defalut: NULL
#' @param skip.columns default: "Seconds"
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return data frame
#' @export
hsGetIwResult <- function (
  file, columns = NULL, skip.columns = "Seconds", dbg = FALSE
)
{
  ## Load result file
  kwb.utils::catIf(dbg, "Reading ", file, "... ")
  df <- utils::read.csv(file, stringsAsFactors = FALSE)
  kwb.utils::catIf(dbg, "ok.\n")
  
  ## Convert timestamp strings to timestamps
  kwb.utils::catIf(dbg, "Converting time strings to timestamps... ")
  df$Time <- as.POSIXct(df$Time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  kwb.utils::catIf(dbg, "ok.\n")
  
  ## Set original column names
  names(df) <- as.character(utils::read.csv(
    file, stringsAsFactors = FALSE, header = FALSE, nrows = 1
  )[1, ])
  
  ## if columns are given, filter for specific columns
  if (!is.null(columns)) {
    df <- df[, names(df) %in% columns]    
  }
  
  ## if columns to skip are given, exclude these columns
  if (!is.null(skip.columns)) {
    idx <- which(names(df) %in% skip.columns)
    if (length(idx) > 0) {
      df <- df[, -idx]      
    }
  }
  
  df
}

# hsIntegrals ------------------------------------------------------------------

#' Integrals
#' 
#' Calculates the integrals (sum of all value columns within event's time 
#'   interval, multiplied with "signal width" of events)
#' 
#' @param data time-series \code{data} with timestamp in first column
#' @param evts event \code{data} describing events contained in \emph{data}
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return vector of integral values with one value per event. Length of vector
#'   corresponds to number of rows in \emph{evts}
#' @export
hsIntegrals <- function(data, evts, dbg = FALSE)
{
  # get signal width from event data
  sigWidth <- kwb.event::hsSigWidth(evts)
  
  kwb.utils::catIf(dbg, "sigWidth:", sigWidth, "\n")
  
  # init vector "eIntegrals" holding one value per event. The value stored
  # is the sum of all values lying within the event (timestamp excluded), 
  # multiplied with the signal width (time period to which one row in data
  # corresponds).
  eIntegrals <- numeric()
  
  # Loop through events
  for (i in 1:nrow(evts)) {
    
    # Get data of current event (timestemp column excluded)
    eData <- kwb.event::hsGetEvent(data, evts, i, useIndex = FALSE)[, -1]
    #cat(sprintf("eData has %d rows and %d columns.\n", 
    #            nrow(eData), ncol(eData)))
    
    if (nrow(eData) > 0) {
      # Calculate sum of all values belonging 
      # to the current event and multiply with the signal width of the events
      eIntegral <- sum(eData, na.rm = TRUE) * sigWidth
    } 
    else {
      eIntegral <- 0 
    }
    
    # cat(sprintf("evt #%2d: integral = %10.2f\n", i, eIntegral))
    eIntegrals <- c(eIntegrals, eIntegral)
  }
  
  eIntegrals
}

# hsIwParNameMap ---------------------------------------------------------------

#' Parameter Name Mapping
#' 
#' Mapping between parameter names used in table names in temporary mdb
#'   (written to by StatAna) and parameter acronyms used in InfoWorks result 
#'   csv files.
#' 
#' @export
hsIwParNameMap <- function() 
{ 
  data.frame(
    mdbName = c("flow", "BOD_tot", "COD_tot", "NH4N", "TKN_tot", "TP_dis", "TP_tot", "TSS"),
    iwrName = c("flow", "bodtot", "codtot", "nh4tot", "tkntot", "tphdis", "tphtot", "sf1"),
    stringsAsFactors = FALSE
  )
}

# hsGetIwResultAvgAboveZero ----------------------------------------------------

#' Get Infoworks Result Average Above Zero
#' 
#' @param src either full path to mdb or to  directory in which InfoWorks result files
#'   (csv) are located
#' @param type one of "flow", "BOD_tot", "COD_tot", "NH4N", "TKN_tot", "TP_dis", 
#'   "TP_tot", "TSS"
#' @export
hsGetIwResultAvgAboveZero <- function(src, type) 
{
  if (length(grep("\\.(mdb|accdb)$", src)) == 1) {
    #cat("Reading data from mdb-file ", src, "...\n")
    data <- hsGetIwResultAvgFromMdb(src, type)    
  }
  else {
    #cat("Reading data from csv-file in directory ", src, "...\n")
    data <- hsGetIwResultAvgFromCsv(src, type)    
  }
  
  # exclude "zero"-rows
  data[rowSums(data[, -1], na.rm = TRUE) > 0, ]  
}

# hsGetIwResultAvgFromMdb ------------------------------------------------------

#' Get Infoworks Result Average From MS Access Database
#' 
#' @param mdb \code{mdb}
#' @param type \code{type}
#' @param tblQ name of database table containing flows
#' @param tblL name of database table containing loads
#' @param skipCols vector of patterns matching names of columns to be skipped
#' @export
hsGetIwResultAvgFromMdb <- function(
  mdb, 
  type,
  tblQ = "tbl_05_Q_bei_Ueberlauf_m3_s", # "tbl_07_Q_inCsoEvents_m3_s"
  tblL = paste("tbl_02_15minMittel_L",  # "tbl_03_15minAvg_L"
               type, "kg_s", sep = "_"),
  skipCols = c("^kug$", "^overall$") # c("Sum*", "kupf_mw")
) 
{  
  # Load mapping between parameter names in tmp mdb and InfoWorks result files
  possTypes <- hsIwParNameMap()$mdbName
  
  if (! (type %in% possTypes))
    stop("type must be one of ", 
         paste("\"", possTypes, "\"", sep = "", collapse = ", "))
  
  #if (type == "flow") {    
  #  tbl <- tblQ
  #} else {    
  #  tbl <- tblL
  #}  
  tbl <- ifelse(type == "flow", tblQ, tblL)
  
  # Get field names of table
  allFields <- kwb.db::hsFields(mdb, tbl)
  
  # Get names of columns to be summed (ignore timstamp and sum columns)
  colNames <- hsNameFilter(allFields[-1], negPtrn = skipCols)

  cat(sprintf("Reading %s from %s in %s...\n", type, tbl, mdb))
  cat(sprintf("%d columns considered: %s\n", length(colNames), 
              paste(colNames, collapse = ",")))
  
  # Get pre-processed data from temp. mdb  
  res <- kwb.db::hsMdbTimeSeries(mdb, tbl, tsField = allFields[1], fields = colNames)
  
  cat("ok.\n")
  
  res
}

# hsGetIwResultAvgFromCsv ------------------------------------------------------

#' Get Infoworks Result Average From CSV
#' 
#' @param csvdir directory in which InfoWorks result files (csv) are located
#' @param type one of "flow", "BOD_tot", "COD_tot", "NH4N", "TKN_tot", "TP_dis", 
#'   "TP_tot", "TSS"
#' @param qthreshold threshold for Q values
#' @param dbg if \code{TRUE}, debug messages are shown
#' @export
hsGetIwResultAvgFromCsv <- function(
  csvdir, type,  qthreshold = 0.003, dbg = TRUE
)
{    
  # Load mapping between parameter names in tmp mdb and InfoWorks result files
  namemap <- hsIwParNameMap()
  
  # init variable saving if data is concentration data instead of mass flows
  is.concentration <- FALSE
  
  if (! (type %in% namemap$mdbName))
    stop("type must be one of ", 
         paste("\"", namemap$mdbName, "\"", sep = "", collapse = ", "))
  
  if (type == "flow") {
    csv <- dir(csvdir, "flow\\.csv$", full.names = TRUE)
    threshold <- qthreshold
  } else {    
    ptrn <- paste("_mf", namemap$iwrName[namemap$mdbName == type], ".csv$", sep = "")
    csv <- dir(csvdir, ptrn, full.names = TRUE)
    
    # if there is no mass flow but only mass concentration remember to multiply
    # with flows!
    if (length(csv) == 0) {
      cat("Could not find", ptrn, "in", csvdir)
      ptrn <- paste("_mc", namemap$iwrName[namemap$mdbName == type], ".csv$", sep = "")
      csv <- dir(csvdir, ptrn, full.names = TRUE)
      is.concentration <- TRUE
    }
    threshold <- NULL
  }
  
  if (length(csv) != 1) 
    stop(ifelse(length(csv) == 0, "No file", "More than one files"), 
         " containing ", type, " data found in ", csvdir, "\n")
  
  ## Get data from csv, selecting only expected columns representing expected
  ## outlets
  #cols <- c("Time", hsIwReferenceOutlets(2)$InfoworksID)
  #data.all <- hsGetIwResult(csv, columns = cols)

  cat(sprintf("Reading %s from %s...\n", type, csv))
  data.all <- hsGetIwResult(csv)
  cat("ok.\n")
  
  ## Calculate 15 min-means
  cat(sprintf("Calculating 15-min means of %s-values... ", type))
  data <- kwb.base::hsGroupByInterval(data.all, interval = 15*60, FUN = mean, 
                            offset1 = -5*60, offset2 = 5*60)
  cat("ok.\n")
  
  ## if threshold is given, discard low values in value matrix
  if (! is.null(threshold)) {
    vm <- data[, -1]
    vm[vm < threshold] <- 0
    
    ## Rewrite value matrix in qdata
    data[, -1] <- vm     
  }
  
  data
}

# hsIwPlot1 --------------------------------------------------------------------

#' Infoworks Plot 1
#' 
#' @param data \code{data}
#' @param allIntegrals \code{allIntegrals}
#' @param evts \code{evts}
#' @param type default: 2
#' @param basemain \code{basemain}
#' @param legend.sort default: FALSE
#' @param ylog default: FALSE
#' @param yBottom default: ifelse(\code{ylog}, 0.0001, 0)
#' @param cex.legend expansion factor for legend texts
#' @export
hsIwPlot1 <- function(
  data, 
  allIntegrals,
  evts,
  type = 2,
  basemain,
  legend.sort = FALSE,
  ylog = FALSE,
  yBottom = ifelse(ylog, 0.0001, 0),
  cex.legend = 0.55
) 
{
  # Loop through result variables plotting integrals per event as calculated
  # before and provided in allIntegrals
  mfpars <- names(allIntegrals)
  
  for (mfpar in mfpars) {
    
    eIntegrals <- allIntegrals[[mfpar]]
    
    # Prepare argument values to be passed to hsTimeBarPlot, 
    # depending on plot type
    if (type == 1) { # Type 1: bar height = integral / duration
      
      height <- eIntegrals / evts$dur    
      
      if (mfpar == "flow") {
        ### volume flows
        main <- paste("Total volume of", basemain)
        ylab <- "mean flow during event in m3/s"
        legend.title <- "Volume V per event:"
        legend.format <- "V = %7.0f m3"
        legend.values <- eIntegrals            
      } else {
        ### mass flows
        main <- paste("Total mass of", mfpar, basemain)
        ylab <- "mean mass flow during event in kg/s"
        legend.title <- "Mass M per event:"
        legend.format <- "M = %7.0f kg"
        legend.values <- eIntegrals 
      }                  
    } else if (type == 2) { # Type 2: bar height = integral
      if (mfpar == "flow") {
        ### volume flows
        height <- eIntegrals / 1000
        main <- paste("Total volume of", basemain)
        ylab <- "total volume discharged during event in 1000 m3"
        legend.title <- "Volume V per event:"
        legend.format <- "V = %7.0f m3"
        legend.values <- eIntegrals    
      } else {
        ### Mass flows
        height <- eIntegrals / 1000
        main <- paste("Total mass of", mfpar, basemain)
        ylab <- "total mass discharged during event in tons"
        legend.title <- "Mass per event:"
        legend.format <- "M = %7.0f kg"
        legend.values <- eIntegrals
      }
    }  
    
    # Barplot with one bar per event and bar height being 
    # either mean (mass) flow or total volume (mass)
    hsTimeBarPlot(
      t1 = evts$tBeg, 
      t2 = evts$tEnd, 
      tlab = "day of event begin", 
      ylog = ylog,
      tlim = c(min(data[[1]]), max(data[[1]])),
      ylim = c(yBottom, max(height)),
      tlab.mindist = 2*86400,
      cex.all = 0.55,
      cex.legend = cex.legend,
      col = ifelse(mfpar == "flow", "lightblue", "grey"),
      height = height, 
      main = main,
      ylab = ylab,
      legend.title = legend.title, 
      legend.format = legend.format,
      legend.values = legend.values,
      legend.sort=legend.sort
    )      
  }
}

# hsIwPlot2 --------------------------------------------------------------------

#' Infoworks Plot 2
#' 
#' @param allIntegrals \code{allIntegrals}
#' @param evts \code{evts}
#' @param plotTotal default: TRUE
#' @param plotEvents default: FALSE
#' @param pars default: c("TSS", "COD_tot", "BOD_tot", "TKN_tot", "NH4N", "TP_tot", "TP_dis")
#' @export
hsIwPlot2 <- function(
  allIntegrals,
  evts,
  plotTotal = TRUE, 
  plotEvents = FALSE, 
  pars = c("TSS", "COD_tot", "BOD_tot", "TKN_tot", "NH4N", "TP_tot", "TP_dis")
) 
{
  # save current setting of graphical parameters
  oldpar <- graphics::par(mfrow = c(1, 1))
  
  # Plot total masses?
  idx <- c()
  if (isTRUE(plotTotal)) {
    idx <- c(idx, 0)
  }
  # Plot for each event?
  if (isTRUE(plotEvents)) {
    idx <- c(idx, 1:nrow(allIntegrals))
  }
  
  for (i in idx) {
    #graphics::barplot(as.matrix(allIntegrals[i, c("COD_tot", "BOD_tot")]), ylab = ylab)
    #graphics::barplot(as.matrix(allIntegrals[i, c("TKN_tot", "NH4N")]), ylab = ylab,
    #        main = sprintf("Event #%d", i))
    #graphics::barplot(as.matrix(allIntegrals[i, c("TP_tot", "TP_dis")]), ylab = ylab)
    
    # i == 0: sum of all events
    if (i == 0) {
      height <- colSums(allIntegrals[, pars]) / 1000
      ylab <- "total mass in t"
      text.format <- "M = %0.1f t"
      hmax <- max(height)
      main <- sprintf(
        "Total masses in %d overflow events between %s and %s", 
        nrow(allIntegrals),
        format.Date(evts$tBeg[1], "%d.%m.%Y"), 
        format.Date(evts$tEnd[nrow(allIntegrals)], "%d.%m.%Y"))
      subtitle <- ""
      if (sum(allIntegrals$COD_tot) != 0) {
        subtitle <- sprintf("BOD/COD = %0.2f", 
                            sum(allIntegrals$BOD_tot) / sum(allIntegrals$COD_tot))
      }
    } else  {
      height <- as.matrix(allIntegrals[i, pars])        
      ylab <- "total mass in kg"
      text.format <- "M = %0.1f kg"
      hmax <- max(height)
      main <- sprintf(
        "Overflow event #%d: %s to %s (%0.1f h)", i,
        format.Date(evts$tBeg[i], "%d.%m.%Y %H:%M"), 
        format.Date(evts$tEnd[i], "%d.%m.%Y %H:%M"), 
        evts$dur[i]/3600)
      subtitle <- ""
      if (allIntegrals$COD_tot[i] != 0) {
        subtitle <- sprintf("BOD/COD = %0.2f", 
                            allIntegrals$BOD_tot[i] / allIntegrals$COD_tot[i])
      }
    }
    #cat("height:\n")
    #print(height)
    
    x <- graphics::barplot(
      height, ylab = ylab, main = main, ylim = c(0, 1.2 * hmax), sub = subtitle
    )
    
    graphics::text(x, height + 0.03*hmax, labels = sprintf(text.format, height))
  }
  
  graphics::par(oldpar)    
}

# hsIwPlot3 --------------------------------------------------------------------

#' Infoworks Plot 3
#' 
#' @param allIntegrals \code{allIntegrals}
#' @param boxplot.range this determines how far the plot whiskers extend out
#'   from the box. See argument \code{range} of \code{\link{barplot}}.
#' @export
hsIwPlot3 <- function(allIntegrals, boxplot.range = 0) 
{
  oldpar <- graphics::par(mfrow = c(1, 2))
  
  graphics::boxplot(
    allIntegrals[, c("COD_tot", "BOD_tot")]/1000, 
    main = "Distribution of COD/BOD discharges per event",
    ylab = "mass per overflow event in t",
    range = boxplot.range
  )
  
  graphics::mtext(sprintf("%d overflow events", nrow(allIntegrals)), side = 3, line = 0)
  
  graphics::boxplot(
    allIntegrals$BOD_tot/allIntegrals$COD_tot, 
    main = "Distribution of BOD/COD ratios",
    ylab = "BOD_tot : COD_tot",
    range = boxplot.range
  )
  
  graphics::mtext(
    sprintf("%d overflow events, mean ratio: %0.2f", 
            nrow(allIntegrals), 
            mean(allIntegrals$BOD_tot/allIntegrals$COD_tot, na.rm = TRUE)),
    side = 3, line = 0
  )
  
  graphics::par(oldpar)
}

# hsIwEventSummary -------------------------------------------------------------

#' Volume and Mass Load per Event
#' 
#' @param src Data source; can be either path to mdb (filled by StatAna) or
#'   directory containing original csv files exported from InfoWorks
#' @param mfpars Vector of parameter acronyms; Default: c("flow", "BOD_tot",
#'   "COD_tot", "NH4N", "TKN_tot", "TP_dis", "TP_tot", "TSS")
#' @return list with event list \emph{evts}, matrix \emph{allIntegrals}
#'   containing volume and mass loads per event and data.frame \emph{iwdata}
#'   containing all result data
#' @export
hsIwEventSummary <- function(src, mfpars = c(
  "flow", "BOD_tot", "COD_tot", "NH4N", "TKN_tot", "TP_dis", "TP_tot", "TSS"
)) 
{
  # init result list
  allIntegrals <- NULL
  
  # Loop through result variables calculating integrals per event  
  for (mfpar in mfpars) {
    
    # Get volume/mass flows of current parameter.
    # If data is taken from mdb, columns that represent sums are excluded.
    # Rows in which the sum over all columns is zero are always excluded.
    iwdata <- hsGetIwResultAvgAboveZero(src, mfpar)    
    
    ## When having read the flows, create events based on timestamps with 
    ## non-zero flow sums
    if (mfpar == "flow") {
      ## MR uses ">=" instead of ">" to compare time difference with event
      ## separation time -> evtSepOp = "ge" (greater than or equal to)
      sigWidth <- kwb.datetime::minTimeStep(iwdata[[1]]) # 15*60
      evts <- kwb.event::hsEvents(
        iwdata[[1]], 
        evtSepTime = 6*3600, 
        signalWidth = sigWidth,
        tUnit = "s", 
        evtSepOp = "ge"
      )
    }
    
    # Calculate integrals per event
    eIntegrals <- hsIntegrals(iwdata, evts)
    
    # Add eIntegrals to list of all integrals
    if (is.null(allIntegrals)) {
      allIntegrals <- data.frame(eIntegrals)
    } else {
      allIntegrals <- cbind(allIntegrals, eIntegrals)
    }
    names(allIntegrals)[ncol(allIntegrals)] <- mfpar
  }
  
  list(evts = evts, allIntegrals = allIntegrals, iwdata = iwdata)
}

# hsIwPlotAll ------------------------------------------------------------------

#' Infoworks Plot All
#' 
#' @param src Data source; can be either path to mdb (filled by StatAna) or
#'   directory containing original csv files exported from InfoWorks
#' @param subtitle \code{subtitle}
#' @param mfpars Vector of parameter acronyms; Default: c("flow", "BOD_tot",
#'   "COD_tot", "NH4N", "TKN_tot", "TP_dis", "TP_tot", "TSS")
#' @param ylog y axis logarithmic?
#' @param type default: 2
#' @param outpdf Path to output file (.pdf). Default: ""
#' @param outdir Path to output directory. Default: ""
#' @param legend.sort default: FALSE
#' @param cex.legend expansion factor for legend texts
#' @export
hsIwPlotAll <- function(
  src,
  subtitle = "",
  mfpars = c("flow", "BOD_tot", "COD_tot", "NH4N", "TKN_tot", "TP_dis", 
             "TP_tot", "TSS"),
  ylog = FALSE,
  type = 2,
  outpdf = "",
  outdir = "", 
  legend.sort = FALSE,
  cex.legend = 0.55
) 
{
  # Provide required data to be plotted
  evtSummary <- hsIwEventSummary(src, mfpars)
  allIntegrals <- evtSummary$allIntegrals
  csoEventList <- evtSummary$evts  
  infoWorksDat <- evtSummary$iwdata

  # Save overview table to database
  if (length(grep("\\.(mdb|accdb)$", src)) == 1) {
    evtStat <- data.frame(eventNr = 1:nrow(csoEventList))
    evtStat <- cbind(evtStat, csoEventList[, 3:4], allIntegrals)
    
    # Rename columns
    substname <- list(flow = "V_m3",
                      BOD_tot = "mBODtot_kg",
                      COD_tot = "mCODtot_kg",
                      NH4N    = "mNH4N_kg",
                      TKN_tot = "mTKNtot_kg",
                      TP_dis  = "mTPdis_kg",
                      TP_tot  = "mTPtot_kg",
                      TSS     = "mTSS_kg")
    
    cnames <- names(evtStat)
    for (sname in names(substname)) {
      cnames[cnames == sname] <- substname[[sname]]
    }
    names(evtStat) <- cnames
    kwb.db::hsPutTable(src, evtStat, "tbl_Stat", overwrite=TRUE) 
  }    
  
  basemain <- paste("overflow events as simulated by InfoWorks", subtitle,
                    sep = "\n")
  yBottom <- ifelse(ylog, 0.0001, 0)
  
  # If path to pdf is given prepare pdf file
  if (outpdf == "") {

    # set default directory if no directory given
    if (outdir == "") {
      if (length(grep("\\.mdb$", src)) == 1) {
        outdir <- dirname(src)
      }
      else {
        outdir <- src
      }      
    }

    # set full file path
    outpdf <- file.path(
      outdir, 
      sprintf("IwResultPlot_%s.pdf", kwb.utils::hsSubstSpecChars(subtitle)))
  }
  
  kwb.utils::hsPrepPdf(outpdf)
  
  # To do: Organise plot area using layout():
  #
  # mat <- matrix(c(1,1,2,
  #                 1,1,3,
  #                 4,4,3), nrow = 3, byrow = TRUE)
  # layout(mat)
  # layout.show(max(mat))  
  hsIwPlot1(data = infoWorksDat, allIntegrals = allIntegrals, 
            evts = csoEventList, type = type,
            basemain = basemain, legend.sort = legend.sort, ylog = ylog,
            yBottom = yBottom, cex.legend = cex.legend)
  
  hsIwPlot2(allIntegrals = allIntegrals, evts=csoEventList, plotTotal = TRUE, 
            plotEvents = FALSE, pars = setdiff(mfpars, "flow"))
  
  hsIwPlot3(allIntegrals = allIntegrals) # boxplot of BOD_tot, COD_tot values
  
  hsIwPlot2(allIntegrals = allIntegrals, evts=csoEventList, plotTotal = FALSE, 
            plotEvents = TRUE, pars = setdiff(mfpars, "flow"))
  
  if (outpdf != "") {
    
    kwb.utils::finishAndShowPdf(outpdf, dbg = FALSE)
  }  
}
