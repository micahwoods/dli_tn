# to estimate DLI for Memphis, Nashville, Chattanooga, & Knoxvile
# first checking Rs for some nearby locations

source("r/libraries.R")
source("r/functions.R")

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

# apply selectFunction to select columns of interest and name them  
all <- selectFunction(rawAll)  

all$date <- ymd(all$date)

# remove the missing data
all <- filter(all, Rs > -5000 &
                minTemp > -5000 &
                maxTemp > -5000 &
                meanTemp > -5000)

# convert from global irradiance to PAR as per Meek, 1984
all$dli <- all$Rs * 2.04


# calculate Ra for these locations
all$Ra <- functionRa(all$latitude, all$date)

# calculate Rs using Hargreaves' radiation formula
all$RsCalc <- hargreave(all$Ra, all$minTemp, all$maxTemp)

# after checking data in various plots in the "r/plots.R" file
# the Rs calculated from Hargreaves equation can be adjusted for these
# regional data

yo <- lm(Rs ~ RsCalc, data = all)
summary(yo)

# based on the regression above, correct the RsCalc
all$RsCalcCorrected <- -2.71108 + all$RsCalc * 1.09444

# based on corrected Rs, estimate a DLI
all$dliCalc <- all$RsCalcCorrected * 2.04
