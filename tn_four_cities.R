# use Hargreaves' equation to estimate Rs (and then DLI)
# for 2015 at Memphis, Nashville, Chattanooga, and Knoxville

# read data file which I've filtered to airports at Memphis, Nashville,
# Knoxville, and Chattanooga
# data are from http://www.ncdc.noaa.gov/cdo-web/

tn <- read.csv("data/tn_2015.csv",
               header = TRUE)

# assign a city name
tn$city <- ifelse(tn$STATION == "GHCND:USW00013882", "Chattanooga",
                  ifelse(tn$STATION == "GHCND:USW00013891", "Knoxville",
                         ifelse(tn$STATION == "GHCND:USW00013893", "Memphis", "Nashville")))

tn$date <- ymd(tn$DATE)

# calculate Ra, the extraterrestrial radiation
tn$Ra <- functionRa(tn$LATITUDE, tn$date)

# adjust temperature decimal point
tn$minTemp <- tn$TMIN / 10
tn$maxTemp <- tn$TMAX / 10

# calculate Rs and then DLI
tn$RsCalc <- hargreave(tn$Ra, tn$minTemp, tn$maxTemp)
tn$dliCalc <- tn$RsCalc * 2.04

tn$date1 <- as.Date(tn$date)

# plot by day of year
p <- ggplot(data = tn, aes(x = date1, y = dliCalc))
p + geom_point(alpha = 0.3) +
  geom_smooth(enp.target = 15, se = FALSE) +
  facet_wrap(~ city, ncol = 2) +
  background_grid(major = "xy") +
  geom_abline(intercept = 40, slope = 0, 
              colour = "dark green", linetype = "dotted") +
  geom_abline(intercept = 32.8, slope = 0, 
              colour = "red", linetype = "dotted") +
  scale_x_date(breaks = "1 month", labels=date_format("%b"),
               expand = c(7/365, 4/365)) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  labs(x = "",
       y = bquote("Estimated daily light integral (DLI) in 2015, mol/"*m^2*"/day")) 

# plot a histogram with 6 bins
p <- ggplot(data = tn, aes(x = dliCalc))
p + geom_histogram(fill = "white", colour = "black",
                   binwidth = 10) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  facet_wrap(~ city, ncol = 2) +
  background_grid(major = "xy", minor = "y") +
  labs(x = bquote("Estimated daily light integral (DLI) in 2015, mol/"*m^2*"/day"),
       y = "Number of days")

# plot a histogram with 6 bins when temp >= 20
tn$meanTemp <- (tn$minTemp + tn$maxTemp) / 2
tnHot <- filter(tn, meanTemp >= 20)

p <- ggplot(data = tnHot, aes(x = dliCalc))
p + geom_histogram(fill = "white", colour = "black",
                   binwidth = 10) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  facet_wrap(~ city, ncol = 2) +
  background_grid(major = "xy", minor = "y") +
  labs(x = bquote("Estimated DLI in 2015 when average daily temperature was at least 68°F, mol/"*m^2*"/day"),
       y = "Number of days")

dliCut40 <- filter(tn, dliCalc >= 40)
dliCut32 <- filter(tn, dliCalc >= 32.8)

summary40 <- dliCut40 %>%
  group_by(city) %>%
  summarise(length(dliCalc))

summary32 <- dliCut32 %>%
  group_by(city) %>%
  summarise(length(dliCalc))

tn$gpC4 <- c4gp(tn$meanTemp)
tn$maxDLI <- tn$Ra * 0.75 * 2.04
tn$dliIndex <- tn$dliCalc / tn$maxDLI
tn$growthIndex <- tn$gpC4 * tn$dliIndex

cha1 <- filter(tn, city == "Chattanooga")
kno1 <- filter(tn, city == "Knoxville")
mem1 <- filter(tn, city == "Memphis")
nas1 <- filter(tn, city == "Nashville")

# adjust for the fewer dates [missing data] of Knoxville
dateVector <- kno1$date

memAdjust <- subset(mem1, date %in% dateVector)
chaAdjust <- subset(cha1, date %in% dateVector)
nasAdjust <- subset(nas1, date %in% dateVector)


cha <- filter(tnHot, city == "Chattanooga")
kno <- filter(tnHot, city == "Knoxville")
mem <- filter(tnHot, city == "Memphis")
nas <- filter(tnHot, city == "Nashville")

# chart of summed DLI when temperature is >= 20
p <- ggplot(data = cha, aes(x = date, y = cumsum(dliCalc)))
p + geom_line(aes(colour = city)) +
  geom_line(data = kno, aes(x = date, y = cumsum(dliCalc),
                            colour = city)) +
  geom_line(data = mem, aes(x = date, y = cumsum(dliCalc),
                            colour = city)) +
  geom_line(data = nas, aes(x = date, y = cumsum(dliCalc),
                            colour = city)) +
  background_grid(major = "xy", minor = "xy") +
  scale_x_datetime(limits = c(ymd(20150315, ymd(20151215))),
                   breaks = date_breaks("2 months")) +
  labs(x = "",
        y = "Cumulative sum of estimated DLI in 2015", 
       title = "Days with average daily temperature of 68°F or above") +
  scale_color_brewer(palette = "Set1")
  

# chart of growth index cumulative through the year, GP * dliCalc/maxDLI

# chart of summed growth index
p <- ggplot(data = chaAdjust, aes(x = date, y = cumsum(growthIndex)))
p + geom_line(aes(colour = city)) +
  geom_line(data = kno1, aes(x = date, y = cumsum(growthIndex),
                            colour = city)) +
  geom_line(data = memAdjust, aes(x = date, y = cumsum(growthIndex),
                            colour = city)) +
  geom_line(data = nasAdjust, aes(x = date, y = cumsum(growthIndex),
                            colour = city)) +
  background_grid(major = "xy", minor = "xy") +
  #scale_x_datetime(limits = c(ymd(20150315, ymd(20151215))),
   #                breaks = date_breaks("2 months")) +
  labs(x = "",
       y = "Cumulative sum of growth index") +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  annotate("text", x = ymd(20150415), y = 90, label = "For more about growth index see\nwww.blog.asianturfgrass.com/2015/09/a-dli-index.html")