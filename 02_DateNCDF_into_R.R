convertDateNcdf2R <- function (time.source, units = "days", origin = as.POSIXct("1800-01-01", 
                                                           tz = "UTC"), time.format = c("%Y-%m-%d", "%Y-%m-%d %H:%M:%S", 
                                                                                        "%Y-%m-%d %H:%M", "%Y-%m-%d %Z %H:%M", "%Y-%m-%d %Z %H:%M:%S")) 
{
  close.file = FALSE
  if (class(time.source) == "character") {
    if (file.exists(time.source)) {
      time.source = open.nc(time.source)
    }
    else {
      stop(paste("File ", time.source, " is not existent!", 
                 sep = ""))
    }
  }
  if (class(time.source) == "NetCDF") {
    attget.result <- try({
      units.file <- infoNcdfAtts(time.source, "time")[, 
                                                      "value"][infoNcdfAtts(time.source, "time")[, 
                                                                                                 "name"] == "units"]
      origin.char <- sub("^.*since ", "", units.file)
      units <- sub(" since.*", "", units.file)
    }, silent = TRUE)
    for (formatT in time.format) {
      origin <- strptime(origin.char, format = formatT, 
                         tz = "UTC")
      if (!is.na(origin)) 
        break
    }
    if (is.na(origin)) 
      stop("Not possible to determine origin. Wrong format supplied?")
    date.vec <- as.numeric(var.get.nc(time.source, "time"))
  }
  else {
    if (!is.numeric(time.source)) 
      stop("time.source needs to be numeric if not a netCDF file connection!")
    date.vec <- time.source
  }
  if (!is.element(units, c("seconds", "minutes", "hours", 
                           "days"))) 
    stop(paste("Unit ", units, " is not implemented.", sep = ""))
  multiplicator <- switch(units, days = 60 * 60 * 24, hours = 60 * 
                            60, minutes = 60, seconds = 1)
  time.out <- origin + date.vec * multiplicator
  if (origin < as.POSIXct("1582-10-30", tz = "UTC")) 
    time.out <- time.out + 10 * 24 * 60 * 60
  if (close.file) 
    close.nc(time.source)
  return(time.out)
}
