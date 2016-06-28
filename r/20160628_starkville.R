# calc estimated DLI for Starkville
# using method described at 
# http://www.seminar.asianturfgrass.com/tn_dli_description.html
# see how the estimates compare with Hodges et al. measurements

source("r/libraries.R")
source("r/functions.R")

stark <- read.csv("data/starkville_201306_to_201409.csv",
               header = TRUE)

stark$date <- ymd(stark$DATE)

stark$LATITUDE <- 33.4692

# calculate Ra, the extraterrestrial radiation
stark$Ra <- functionRa(stark$LATITUDE, stark$date)

# adjust temperature to C -- these downloaded were in F
stark$minTemp <- (stark$TMIN - 32) * 5/9
stark$maxTemp <- (stark$TMAX - 32) * 5/9

# calculate Rs 
stark$RsCalc1 <- hargreave(stark$Ra, stark$minTemp, stark$maxTemp)

# adjust Rs based on the 8 stations in region from NRCC data
stark$RsCalc <- -2.71108 + stark$RsCalc1 * 1.09444

# and then calculate DLI
stark$dliCalc <- stark$RsCalc * 2.04

# now filter data to specific dates

year1 <- filter(stark, date >= ymd(20130613) & date <= ymd(20130929))
year2 <- filter(stark, date >= ymd(20140602) & date <= ymd(20140927))

bothYear <- rbind.data.frame(year1, year2)

mean(bothYear$dliCalc)
