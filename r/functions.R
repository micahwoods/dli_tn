# define functions for this project

# for the NRCC datasets, this selects the columns of interest and names them

selectFunction <- function(x) {
  selectedCols <- x[, c(2, 5, 6, 7, 8, 11, 29)]
  colnames(selectedCols) <- c("date", "latitude", "maxTemp", "minTemp",
                              "meanTemp", "Rs", "location")
  return(selectedCols)
}


# calculate Ra based on input of latitude and day of year

functionRa <- function(x, y) {
  
  # gives first constant
  k.1 <- (24*60)/pi  
  
  # the solar constant
  solar.constant <- 0.0820  
  
  day.of.year <- yday(y) # gives the day of the year as an integer
  
  # next lines give inverse relative distance Earth-Sun and solar declination
  inverse.distance <- 1 + 0.033 * cos(((2 * pi) / 365) * day.of.year)
  solar.declination <- 0.409 * sin((((2 * pi) / 365) * day.of.year) - 1.39)
  
  latitude.radians <- (pi / 180) * (x) # convert latitude to radians
  sunset.hour.angle <- acos(-tan(latitude.radians) * tan(solar.declination))
  
  # the following equation now calculates the Ra, extraterrestrial irradiance
  R.a <- ((k.1 * solar.constant) * inverse.distance) *
    (sunset.hour.angle * sin(latitude.radians) * sin(solar.declination) +
       cos(latitude.radians) * cos(solar.declination) * sin(sunset.hour.angle))
  
  return(R.a)
}

# the estimated Rs for a location based on Hargreave
# where x = Ra, y = min temp, and z = max temp

hargreave <- function(x, y, z) {
  R.s <- 0.16 * sqrt(z - y) * x
  return(R.s)
}

# function to calculate gp C4
c4gp <- function(x) {
  GP <- ifelse(x >= 31, 1, 2.71828 ^ (-0.5 * ((x - 31) / 7) ^ 2))
  return(GP)
}