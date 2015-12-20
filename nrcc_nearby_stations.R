# to estimate DLI for Memphis, Nashville, Chattanooga, & Knoxvile
# first checking Rs for some nearby locations

library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

hol <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-MS_Holly_Springs_4_N.txt",
                  header = FALSE)

hol$loc <- "Holly Springs, MS"

gad <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-AL_Gadsden_19_N.txt",
                  header = FALSE)

gad$loc <- "Gadsden, AL"

wat <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-GA_Watkinsville_5_SSE.txt",
                  header = FALSE)

wat$loc <- "Watkinsville, GA"
  
bat <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-AR_Batesville_8_WNW.txt",
                  header = FALSE)

bat$loc <- "Batesville, AR"

cro <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-TN_Crossville_7_NW.txt",
                  header = FALSE)

cro$loc <- "Crossville, TN"
  
cha <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-VA_Charlottesville_2_SSE.txt",
                  header = FALSE)

cha$loc <- "Charlottesville, VA"
  
ash <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-NC_Asheville_8_SSW.txt",
                  header = FALSE)

ash$loc <- "Asheville, NC"
  
bow <- read.table("http://www1.ncdc.noaa.gov/pub/data/uscrn/products/daily01/2015/CRND0103-2015-KY_Bowling_Green_21_NNE.txt",
                  header = FALSE)

bow$loc <- "Bowling Green, KY"

rawAll <- rbind.data.frame(hol, wat, bat, cro, cha, ash, bow, gad)
  
selectFunction <- function(x) {
  selectedCols <- x[, c(2, 5, 6, 7, 8, 11, 29)]
  colnames(selectedCols) <- c("date", "latitude", "maxTemp", "minTemp",
                             "meanTemp", "Rs", "location")
  return(selectedCols)
}
  
all <- selectFunction(rawAll)  

all$date <- ymd(all$date)

all <- filter(all, Rs > -5000 &
                minTemp > -5000 &
                maxTemp > -5000 &
                meanTemp > -5000)

all$dli <- all$Rs * 2.04

p <- ggplot(data = all, aes(x = date, y = dli))
p + geom_point() +
  facet_wrap(~ location) +
  background_grid(major = "xy", minor = "xy")

p <- ggplot(data = all, aes(x = dli))
p + geom_histogram(fill = "white", colour = "black") +
  facet_wrap(~ location, ncol = 2)

hot <- filter(all, meanTemp >= 24)
  
p <- ggplot(data = hot, aes(x = dli))
p + geom_histogram(fill = "white", colour = "black") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  facet_wrap(~ location, ncol = 2)


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

all$Ra <- functionRa(all$latitude, all$date)

# now to estimate DLI based on
# the estimated Rs for a location based on Hargreave
# where x = Ra, y = min temp, and z = max temp

hargreave <- function(x, y, z) {
  
  R.s <- 0.16 * sqrt(z - y) * x
  return(R.s)
}

all$RsCalc <- hargreave(all$Ra, all$minTemp, all$maxTemp)

p <- ggplot(data = all, aes(x = RsCalc, y = Rs))
p + geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, method = 'lm') + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  facet_wrap(~ location, ncol = 4) +
  scale_x_continuous(limits = c(-1, 32)) +
  scale_y_continuous(limits = c(-1, 32)) +
  background_grid(major = "xy")

yo <- lm(Rs ~ RsCalc, data = all)

summary(yo)

all$RsCalcCorrected <- -2.71108 + all$RsCalc * 1.09444

# what I am really interested in -- the distribution of DLI
# is it roughly same in actual Rs, and in RsCalc, location by location?

p <- ggplot(data = all, aes(x = Rs))
p + geom_histogram(fill = "red", colour = "red", alpha = 0.5) +
  geom_histogram(data = all, aes(x = RsCalc),
                 fill = "blue", colour = "blue", alpha = 0.5) +
  facet_wrap(~ location, ncol = 4)
