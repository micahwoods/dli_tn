# some charts looking at the data

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


p <- ggplot(data = all, aes(x = RsCalc, y = Rs))
p + geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, method = 'lm') + 
  geom_abline(intercept = 0, slope = 1, colour = "dark green") +
  facet_wrap(~ location, ncol = 4) +
  scale_x_continuous(limits = c(-1, 32)) +
  scale_y_continuous(limits = c(-1, 32)) +
  background_grid(major = "xy")

# what I am really interested in -- the distribution of DLI
# is it roughly same in actual Rs, and in RsCalc, location by location?

p <- ggplot(data = all, aes(x = Rs))
p + geom_histogram(fill = "blue", colour = "blue", alpha = 0.5) +
  geom_histogram(data = all, aes(x = RsCalc),
                 fill = "red", colour = "red", alpha = 0.5) +
  facet_wrap(~ location, ncol = 4)

# look at histogram with few bins, like 6 bins
# should show if roughly equivalent estimates
p <- ggplot(data = all, aes(x = dli))
p + geom_histogram(fill = "blue", colour = "blue", alpha = 0.5,
                   binwidth = 10) +
  geom_histogram(data = all, aes(x = dliCalc),
                 fill = "red", colour = "red", alpha = 0.5,
                 binwidth = 10) +
  facet_wrap(~ location, ncol = 4)

# look at histogram with few bins only during summer, or hot weather
hot <- filter(all, meanTemp >= 20)

p <- ggplot(data = hot, aes(x = dli))
p + geom_histogram(fill = "blue", colour = "blue", alpha = 0.5,
                   binwidth = 10) +
  geom_histogram(data = hot, aes(x = dliCalc),
                 fill = "red", colour = "red", alpha = 0.5,
                 binwidth = 10) +
  facet_wrap(~ location, ncol = 4)

# plot DLI actual through 2015 faceted by location
p <- ggplot(data = all, aes(x = date, y = dli))
p + geom_point() +
  geom_smooth(enp.target = 15, se = FALSE) +
  facet_wrap(~ location, ncol = 4) +
  background_grid(major = "xy", minor = "xy")

# plot DLI actual through 2015 faceted by location
# and add in the DLI calc moving average and see how it compares
p <- ggplot(data = all, aes(x = date, y = dli))
p + geom_point(alpha = 0.3) +
  geom_smooth(enp.target = 15, se = FALSE) +
  geom_smooth(data = all, aes(x = date, y = dliCalc),
              enp.target = 15, se = FALSE, colour = "Red") +
  facet_wrap(~ location, ncol = 4) +
  background_grid(major = "xy", minor = "xy") +
  scale_x_datetime(limits = c(ymd(20150101), ymd(20151220))) +
  labs(x = "",
       y = "DLI, points are measured, blue is moving avg, red is moving avg of estimate")

# plot DLI actual through 2015 faceted by location and cut by being 20 C or over
# and add in the DLI calc moving average and see how it compares
p <- ggplot(data = hot, aes(x = date, y = dli))
p + geom_point(alpha = 0.3) +
  geom_smooth(enp.target = 10, se = FALSE) +
  geom_smooth(data = hot, aes(x = date, y = dliCalc),
              enp.target = 10, se = FALSE, colour = "Red") +
  facet_wrap(~ location, ncol = 4) +
  background_grid(major = "xy", minor = "xy")

