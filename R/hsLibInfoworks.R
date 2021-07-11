# hsRep ------------------------------------------------------------------------

#' Repeat \code{elements} n-times
#' 
#' @param elements vector of which each element is to be repeated n-times
#' @param n number by which each element of \emph{elements} is repeated
#' 
#' @return vector in which each element of \emph{elements} is repeated \emph{n}-times
#' 
hsRep <- function(elements, n) 
{
  res <- NULL
  for (ele in elements) {
    res <- c(res, rep(ele, n))
  }
  res
}

# hsGerrisConstParsMia ---------------------------------------------------------

#' Gerris Constant Parameters
#' 
#' Gerris/QSim boundary conditions that are treated as constant within MIA-CSO
#'   project. Parameter names according to definition in Gerris configuration
#'   file "GerrisParam.xml"
#' 
#' @param VO2 "Sauerstoffgehalt" in mg/l
#' @param SI "Silizium" in mg/l
#' @param VPH "pH-Wert"
#' @param VX0 "Nitrosomonas" in mg/l
#' @param VX02 "Nitrobacter" in mg/l
#' @param ZOOIND "Rotatorien/Zooplanktondichte" in Ind/l
#' @param VKIGR "Kieselalgen/Anteil der Kieselalgen am Gesamt-Chlorophyll-a", 0..1
#' @param LF Leitfaehigkeit" in mikro-S/cm
#' @param MW "m-Wert" in mmol/l
#' @param VNO2 "Nitrit-N" in mg/l
#' @param VNO3 "Nitrat-N" in mg/l
#' @param CA "Calcium" in mg/l
#' @param CHLA "Chlorophyll-a" in mikro-g/l
#' @param ANTBL "Blaualgen/Anteil der Blaualgen am Gesamt-Chlorophyll-a", 0..1
#' 
#' @return Data frame with columns \emph{VO2}, \emph{SI}, \emph{VPH}, \emph{VX0},
#'   \emph{VX02}, \emph{ZOOIND}, \emph{VKIGR}, \emph{LF}, \emph{MW},
#'   \emph{VNO2}, \emph{VNO3}, \emph{CA}, \emph{CHLA}, \emph{ANTBL} containing
#'   the default values for non-simulated parameters in the first and only row
#' 
hsGerrisConstParsMia <- function
(
  VO2    = 0, 
  SI     = 0, 
  VPH    = 7.44,
  VX0    = 0, 
  VX02   = 0, 
  ZOOIND = 0,
  VKIGR  = 0.33,
  LF     = 293.33,
  MW     = 1.39,
  VNO2   = 0.21,
  VNO3   = 1.1,
  CA     = 36.04,
  CHLA   = 0,
  ANTBL  = 0.33
)
{
  data.frame(VO2,    SI,    VPH,  VX0, VX02, ZOOIND, VKIGR, LF,   MW,  VNO2, 
             VNO3,   CA,    CHLA, ANTBL)
}

# hsGerrisConstBoundMatrix -----------------------------------------------------

#' Gerris Constant Boundary Matrix
#' 
#' Character matrix containing default values for non-simulated parameters
#' 
#' @param boundNames names of locations at which boundary conditions are given
#' @param constPars values of parameters that are treated as constant boundary conditions
#' @param tstamp.1 first timestamp
#' @param tstamp.2 second timestamp
#' @param tstamp.n last timestamp
#' 
hsGerrisConstBoundMatrix <- function(
  boundNames,
  constPars,
  tstamp.1,
  tstamp.2,
  tstamp.n
)
{
  ## number of locations at which boundary conditions are given
  nBounds <- length(boundNames)
  
  ## number of constant parameters
  ncp <- ncol(constPars)
  
  ## repeat constant parameter values
  vals <- hsRep(constPars[1, ], nBounds)
  
  ## Prepare an empty character matrix
  mat <- matrix("", nrow = 5, ncol = 1 + ncp * nBounds)
  
  ## Fill the matrix
  mat[1, ] <- c("Zeitstempel", rep(boundNames, ncp))
  mat[2, ] <- c("", hsRep(names(constPars), nBounds))
  mat[3, ] <- c(tstamp.1, vals)
  mat[4, ] <- c(tstamp.2, vals)
  mat[5, ] <- c(tstamp.n, vals)
  
  mat
}

# hsCreateGerrisInputConstParsCsv ----------------------------------------------

#' Create Gerris Input File for Constant Parameters
#' 
#' Create Gerris input file containing boundary conditions for non-simulated
#'   water quality parameters
#' 
#' @param csvExample path to example csv file containing names of Gerris boundary conditions
#'   in first row, starting in second column (first column is timestamp)  
#' @param csvOut full path to csv file to which non-simulated parameters are to be written.
#'   If missing, this function returns a character matrix containing the
#'   file content.
#' @param constParVals Data frame with columns \emph{VO2}, \emph{SI}, \emph{VPH}, \emph{VX0},
#'   \emph{VX02}, \emph{ZOOIND}, \emph{VKIGR}, \emph{LF}, \emph{MW},
#'   \emph{VNO2}, \emph{VNO3}, \emph{CA}, \emph{CHLA}, \emph{ANTBL} containing
#'   the values for non-simulated parameters in the first and only row.
#'   Default: result of hsGerrisConstParsMia().
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
hsCreateGerrisInputConstParsCsv <- function(
  csvExample,
  csvOut = NULL,
  constParVals = hsGerrisConstParsMia(),
  dbg = TRUE
)
{
  ## Read example csv file
  df <- utils::read.csv2(csvExample, stringsAsFactors = FALSE, header = FALSE)
  
  ## Get names of boundary conditions from first line
  boundNames <- unique(as.character(df[1, -1]))
  
  ## Create character matrix of parameter values
  mat <- hsGerrisConstBoundMatrix(boundNames, constParVals, tstamp.1 = df[3, 1], 
                                tstamp.2 = df[4, 1], tstamp.n = df[nrow(df), 1])    
  
  if (is.null(csvOut)) {
    return(mat)
  } else {
    ## if only a file name is given write the file into the folder of the 
    ## example file
    if (dirname(csvOut) == ".") {
      
      kwb.utils::catIf(
        dbg, "No directory given for the output file.", 
        "File will be created in the directory of the example file.\n"
      )
      
      csvOut <- file.path(dirname(csvExample), csvOut)
    }
    
    kwb.utils::catIf(dbg, "Writing", csvOut, "...")
    
    utils::write.table(
      mat, file = csvOut, sep = ";", quote = FALSE, row.names = FALSE, 
      col.names = FALSE
    )
    
    kwb.utils::catIf(dbg, "ok.\n")
  }
}

# hsCreateGerrisInputFile ------------------------------------------------------

#' Write File for Gerris Import
#' 
#' Write a file for Gerris import based on a database table generated by
#'   StatAnalysis-evaluation "IwToQSim".
#' 
#' @param mdb full path to MS Access database containing table with required
#'   data
#' @param tbl name of table to be exported to csv file
#' @param gerrisParID Gerris parameter ID: one of "OBSB", "OCSB", "VNH4",
#'   "GESN", "GELP", "GESP", "SS", "Q"
#' @param csv.dir path to directory to which output file is to be written
#' @param csv.file optional name for output file, default:
#'   "forGerrisImport_<gerrisParID>.csv"
#' @param writeFile only if TRUE, a file is written, otherwise the corresponding
#'   data is only returned by this function but not written to file.
#' @param tsFormat format of timestamp to be used in output file (\%d = day, \%m =
#'   month, \%Y = year, \%H = hour, \%M = minute, \%S = second)
#' @param subst.na string value by which NULL values in table are subsubstituted
#'   in the output file
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
hsCreateGerrisInputFile <- function(
  mdb,
  tbl,
  gerrisParID,
  csv.dir,
  csv.file = paste0("forGerrisImport_", gerrisParID, ".csv"),
  writeFile = TRUE,
  tsFormat = .defaultTimeFormat(),
  subst.na = "",
  dbg = FALSE
)
{
  ## Read time series from database
  kwb.utils::catIf(
    dbg, sprintf("Reading %s-series from %s...\n", gerrisParID, tbl)
  )
  
  df <- kwb.db::hsMdbTimeSeries(mdb, tbl, dbg = FALSE)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  ## Get row and column number
  nr <- nrow(df)
  nc <- ncol(df)
  
  ## Format output
  df[, 1]    <- format(df[, 1], tsFormat) 
  df[, 2:nc] <- round(df[, 2:nc], 3)
  
  ## Insert row with qsim parameter id
  df.out <- rbind(c(NA, rep(gerrisParID, nc)), df)    
  
  ## write output csv file if writing is requested
  if (writeFile) {
    
    ## Path to output csv file
    csv.path <- file.path(csv.dir, csv.file)
    
    kwb.utils::catIf(dbg, sprintf("Writing %s... ", csv.path))
    
    utils::write.table(
      df.out, csv.path, sep = ";", row.names = FALSE, quote = FALSE, 
      na = subst.na
    )
    
    kwb.utils::catIf(dbg, "ok.\n")
  }
  
  ## Return data frame
  df.out
}

# .defaultTimeFormat -----------------------------------------------------------
.defaultTimeFormat <- function()
{
  "%d.%m.%Y %H:%M"
}

# hsCreateGerrisInputFiles -----------------------------------------------------

#' Create Gerris Input Files
#' 
#' Create Gerris input files from database containing tables prepared by 
#'   StatAna-Evaluation "eIwToQSim"
#' 
#' @param mdb full path to MS Access database (.mdb) containing tables prepared
#'   by StatAna-Evaluation "eIwToQSim"
#' @param csv.dir output directory to which database tables are to be exported
#'   in CSV format
#' @param tblQ name of table containing flows Q in m3/s; default: "tbl_Q_m3_s"
#' @param tblBOD name of table containing BOD concentrations in mg/L; default:
#'   "tbl_c_BOD_tot_mg_L"
#' @param tblCOD name of table containing COD concentrations in mg/L; default:
#'   "tbl_c_COD_tot_mg_L"
#' @param tblNH4N name of table containing NH4-N concentrations in mg/L;
#'   default: "tbl_c_NH4N_mg_L"
#' @param tblNges name of table containing N total concentrations in mg/L;
#'   default: "tbl_c_NGES_mg_L"
#' @param tblTPdis name of table containing total P (dissolved) concentrations
#'   in mg/L; default: "tbl_c_TP_dis_mg_L"
#' @param tblTPtot name of table containing total P concentrations in mg/L;
#'   default: "tbl_c_TP_tot_mg_L"
#' @param tblTSS name of table containing TSS concentrations in mg/L; default:
#'   "tbl_c_TSS_mg_L"
#' @param csv.basename basename of file(s) to be created in \emph{csv.dir};
#'   default: "forGerrisImport". For flows, "_Hydrax" will be appended to the
#'   basename, and for concentrations of water quality parameters "_QSim_<par>",
#'   where <par> is one of "OBSB", "OCSB", "VNH4", "GESN", "GELP", "GESP", "SS".
#'   To the file containing concentrations of all the water quality parameters
#'   "_QSim_all" is appended.
#' @param separate if TRUE, one file per parameter is created.
#' @param overall if TRUE, one file containing all parameters is created.
#' @param tsFormat format of timestamp to be used in output file (\%d = day, \%m
#'   = month, \%Y = year, \%H = hour, \%M = minute, \%S = second)
#' @param subst.na substitution value for NA values
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
hsCreateGerrisInputFiles <- function(
  mdb, 
  csv.dir,
  tblQ = "tbl_Q_m3_s",
  tblBOD = "tbl_c_BOD_tot_mg_L",
  tblCOD = "tbl_c_COD_tot_mg_L", 
  tblNH4N = "tbl_c_NH4N_mg_L",
  tblNges = "tbl_c_NGES_mg_L",
  tblTPdis = "tbl_c_TP_dis_mg_L",
  tblTPtot = "tbl_c_TP_tot_mg_L",    
  tblTSS = "tbl_c_TSS_mg_L",
  csv.basename = "forGerrisImport",
  separate = TRUE,
  overall = TRUE,
  tsFormat = .defaultTimeFormat(),
  subst.na = "",
  dbg = FALSE
)
{
  ## assign table names to QSim parameter acronyms
  gid2tbl <- list(OBSB = tblBOD,
                  OCSB = tblCOD,
                  VNH4 = tblNH4N,
                  GESN = tblNges,
                  GELP = tblTPdis,
                  GESP = tblTPtot,
                  SS   = tblTSS)
  
  ## Reset data frame holding all data
  df.all <- NULL
  
  ## Loop through tables for which csv files are to be created
  for (gid in names(gid2tbl)) {
    
    ## Lookup table name
    tbl <- gid2tbl[[gid]]
    
    ## if table name is given, get data from table and join with df.all
    if (tbl != "") {
      df.out <- hsCreateGerrisInputFile(
        mdb, 
        tbl, 
        gerrisParID = gid, 
        csv.dir = csv.dir, 
        csv.file = sprintf("%s_QSim_%s.csv", csv.basename, gid), 
        writeFile = separate, 
        tsFormat = tsFormat, 
        subst.na = subst.na, 
        dbg = dbg) 
      
      if (overall) {
        if (is.null(df.all)) {
          df.all <- df.out
        }
        else {
          df.all <- cbind(df.all, df.out[, -1])
        }
      }              
    }
  } # end of for-loop
  
  ## write all data in one csv file if requested
  if (overall) {
    
    csv.path <- file.path(csv.dir, sprintf("%s_QSim_all.csv", csv.basename))
    
    kwb.utils::catIf(dbg, "Writing", csv.path, "...\n")
    
    utils::write.table(
      df.all, csv.path, sep = ";", row.names = FALSE, quote = FALSE,
      na = subst.na
    )
    
    kwb.utils::catIf(dbg, "ok.\n")
  }  

  # Write file for Q separately
  if (tblQ != "") {
    res <- hsCreateGerrisInputFile(
      mdb, 
      tblQ, 
      gerrisParID = "Q", 
      csv.dir = csv.dir, 
      csv.file = sprintf("%s_Hydrax.csv", csv.basename), 
      writeFile = TRUE, 
      tsFormat = tsFormat, 
      subst.na = subst.na, 
      dbg = dbg
    )
  }    
}

# hsRowLen ---------------------------------------------------------------------

#' Row Length in InfoWorks Result CSV File
#' 
#' length of a data row in the InfoWorks result CSV file in bytes
#' 
#' @param colWidth width of a data column in bytes
#' @param colNum number of data columns
#' @param tsFormat string representing a timestamp
#' 
#' @return Number of bytes needed for one row of an InfoWorks result CSV file with
#'   \emph{colNum} data columns of \emph{colWidth} bytes each and a timestamp
#'   column of format according to the example timestamp \emph{tstamp}. 
#' 
hsRowLen <- function(colWidth, colNum, tsFormat = "2011-12-31 23:59:59") 
{
  ## 21 is the number of bytes needed for the extra column counting the seconds
  ## since simulation begin that is always contained in an InfoWorks result CSV file.
  (colWidth + 1) * colNum + nchar(tsFormat) + 21 + 2 # inclusive EOL
}

# hsFileSize -------------------------------------------------------------------

#' Size of InfoWorks Result CSV File
#' 
#' Size of an InfoWorks result CSV file in bytes
#' 
#' @param nDays number of days
#' @param bytesHeader length of header line in bytes
#' @param bytesRow length of data row in bytes
#' @param timestep result \code{timestep} in seconds
#' 
#' @return Size of an InfoWorks result CSV file over \emph{nDays} with a result
#'   \code{timestep} of \emph{timestep} seconds in bytes if the header file 
#'   is \emph{bytesHeader} and each data row is \emph{bytesRow} bytes long.
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
hsFileSize <- function(nDays, bytesHeader, bytesRow, timestep, dbg = FALSE) 
{
  # one last line for 00:00:00 of last day
  nRows <- nDays * (24*60*60) / timestep + 1
  
  kwb.utils::catIf(dbg, "Number of rows:", nRows, "\n")
  
  nRows * bytesRow + bytesHeader
}

# hsDaysInFile -----------------------------------------------------------------

#' Number of Days in InfoWorks Result CSV File
#' 
#' @param bytesFile file length in bytes
#' @param bytesHeader length of header line in bytes
#' @param bytesRow length of data row in bytes
#' @param timestep result \code{timestep} in seconds
#' 
#' @return number of days \dQuote{contained} in an InfoWorks result CSV file of size
#'   \emph{bytesFile} with a header line of \emph{bytesHeader} bytes length, 
#'   each data row being \emph{bytesRow} bytes long and a result \code{timestep} of
#'   \emph{timestep} seconds.
#' 
hsDaysInFile <- function(bytesFile, bytesHeader, bytesRow, timestep) 
{
  ((bytesFile - bytesHeader) / bytesRow - 1) * timestep / (24 * 60 * 60)
}

# hsTsInFile -------------------------------------------------------------------

#' Result Time Step in InfoWorks Result CSV File
#' 
#' @param bytesFile file length in bytes
#' @param bytesHeader length of header line in bytes
#' @param bytesRow lengh of data row in bytes
#' @param nDays number of days
#' 
#' @return Returns the (possibly) applied result timestep of an InfoWorks simulation 
#'   depending on the maximal allowed InfoWorks result CSV file size \emph{bytesFile}
#'   in bytes, on the number \emph{nDays} of days to simulate and on the size
#'   \emph{bytesHeader} and \emph{bytesRow} of the header and of a data
#'   line, respectively.
#' 
hsTsInFile <- function(bytesFile, bytesHeader, bytesRow, nDays) 
{
  (24 * 60 * 60) * nDays / ((bytesFile - bytesHeader) / bytesRow - 1)
}

# hsIwResultFileSize -----------------------------------------------------------

#' InfoWorks Result CSV File Size
#' 
#' Size of an InfoWorks result CSV file depending on the simulated time period
#'   between \emph{dateFirst} and \emph{dateLast}, the result \code{timestep} 
#'   \emph{timestep} applied and the number \emph{nDataCol} of data columns in 
#'   the file.
#' 
#' @param dateFirst first date (day) to be simulated in ISO-format: \emph{yyyy-mm-dd}
#' @param dateLast last date (day) to be simulated in ISO-format: \emph{yyyy-mm-dd}
#' @param timestep result \code{timestep} in seconds
#' @param nDataCol number of data columns (time-columns excluded) in the InfoWorks result CSV file
#' @param bytesHeader length of header line in bytes
#' @param tstamp string representing an example timestamp
#' @param colWidth width of a data column in bytes
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return List with elements \emph{Bytes}, \emph{kB}, \emph{MB}, \emph{GB}
#'   giving the requested file size in the according unit.
#' 
hsIwResultFileSize <- function(
  dateFirst,
  dateLast,
  timestep,
  nDataCol,
  bytesHeader = -1,
  tstamp = "yyyy-mm-dd hh:nn:ss",
  colWidth = 12,
  dbg = FALSE
) 
{
  nDays <- as.integer(as.Date(dateLast) - as.Date(dateFirst))

  chrsPerRow <- hsRowLen(colWidth, nDataCol, tstamp)
  
  kwb.utils::catIf(dbg, "Characters per row:", chrsPerRow, "\n")
  
  # If we do not have the exact number of header bytes, assume the header line
  # to be as long as a data line
  if (bytesHeader == -1) bytesHeader = chrsPerRow
  
  nBytes <- hsFileSize(nDays, bytesHeader, chrsPerRow, timestep, dbg)
  list(Bytes = nBytes    / (div <- 1), 
    kB = round(nBytes / (div <- div * 1024), 1), 
    MB = round(nBytes / (div <- div * 1024), 1),
    GB = round(nBytes / (div <- div * 1024), 1))
}

# hsPlotIwFileSizeVsTsAndPeriod ------------------------------------------------

#' IW File Size vs. Time Step, Period
#' 
#' Plot showing InfoWorks result CSV file size vs. different combinations of
#'   timestep and simulated time period
#' 
#' @param nCols number of data columns in the InfoWorks result CSV file
#' @param colWidth width of a data column in bytes
#' @param tsFormat string representing a timestamp
#' @param bytesHeader length of header line in bytes
#' 
hsPlotIwFileSizeVsTsAndPeriod <- function(
  nCols,
  colWidth = 12,
  tsFormat = "01.01.2011 00:00:00",
  bytesHeader = -1
) 
{
  bytesRow <- hsRowLen(colWidth, nCols, tsFormat) # bytes per row

  tsteps     <- c(  15,    30,     60,    120,    300,     600,    1500 )
  tstepNames <- c("15s", "30s", "1min", "2min", "5min", "10min", "15min")
  daynums    <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Start with file sizes for 1 day
  myBs <- hsFileSize(1, bytesHeader, bytesRow, tsteps)
  myBs <- 1024 * 1024 * 2^(0:10)
  
  # Positions and labels of x-axis ticks
  myAtX  <- 1024^2 * 2^c(0:11)
  myAtXL <- c(paste(2^(0:9), "MB"), "1 GB", "2 GB")  
  
  # Create a plot for each month
  graphics::par(oma = c(1,1,1,1), mar = c(4,4,5,1))
  
  for (i in seq(12, 1, by = -1)) {
    nDays <- sum(daynums[1:i])
    myBs  <- hsFileSize(nDays, bytesHeader, bytesRow, tsteps)
    myLty <- ifelse(i %% 2 == 0, 1, 2)
    
    if (i == 12) {
      
      graphics::plot(myBs, tsteps, 
        main = sprintf("Size of InfoWorks result CSV file with %d data columns",
          nCols),
        xlim = c(1*1024^2, 2*1024^3), 
        ylim = c(15, 3000), # 1500), 
        type = "l",
        log = "xy", 
        yaxt = "n", 
        xaxt = "n", 
        lty = myLty, 
        xlab = "CSV file size", 
        ylab ="Result time step")
      graphics::axis(2, at = tsteps,   labels = tstepNames)
      graphics::axis(1, at = myAtX, labels = myAtXL)
      # create a grid manually
      graphics::abline(h = tsteps, v = myAtX, lty = 3, col = "gray")
      
    } else {
      
      graphics::lines(myBs, tsteps, ylim = c(15, 1500), type = "l", lty = myLty)
    }
    
    # adj=0: left-justified
    if (i < 6 | (i %% 2 == 0)) {
      
      graphics::text(myBs[length(tsteps)], 1500*1.05, paste(i, "months"), 
        srt = 90, adj = 0, xpd = TRUE, cex = 0.9)
    }
  }
}
