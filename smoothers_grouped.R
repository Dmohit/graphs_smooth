#Install the required packages
#install.packages(c('hexbin', 'RColorBrewer', 'scales', 'ggplot2', 'microbenchmark',
#                    'tidyr', 'dplyr'))

library(RColorBrewer)
library(ggplot2)
library(scales)
library(KernSmooth)
library(tidyr)
library(dplyr)

data(diamonds)

mydiamonds <- data.frame(lprice = log(diamonds$price, 2),
                            lcarat = log(diamonds$carat, 2))

# calculate lm
mylm <- lm(lprice ~ lcarat, data = mydiamonds)

# calculate NW kernel
myks <- ksmooth(x = mydiamonds$lcarat, y = mydiamonds$lprice,
                    kernel = 'normal', n.points = 200)

myls2 <- loess(mydiamonds$lprice ~ mydiamonds$lcarat,
                        control =
                        loess.control(surface = "interpolate",
                                    statistics = "approximate",
                                    trace.hat = "approximate", cell = 0.2))

# calculate alternate smoother (also NW kernel, but using different defaults)
h <- dpill(x = mydiamonds$lcarat, y = mydiamonds$lprice)
fit <- locpoly(x = mydiamonds$lcarat, y = mydiamonds$lprice, bandwidth = h)

# prepare loess smoother for plotting
ord <- order(myls2$x)

Data = data.frame(rbind(mydiamonds,
                        data.frame(lcarat = myks$x, lprice = myks$y),
                        data.frame(lcarat = myls2$x[ord], lprice = myls2$fitted[ord])))

Data$smooth <- c(rep('S', nrow(mydiamonds)),
                    rep('NW', length(myks$x)),
                    rep('Loess', length(myls2$x)))

Data$smooth <- factor(Data$smooth)

my_colors1 = brewer.pal(7, 'Blues')
my_colors2 = c("NW" = "red", "Loess" = "seagreen")
my_lines = c('NW' = 1, 'Loess' = 2)

##First the hexbin plot
d <- ggplot(Data) +
    geom_hex(aes(lcarat, lprice), binwidth = c(.15, .15),
                    data = . %>% filter(smooth == 'S')) +
    scale_fill_continuous(low = my_colors1[2], high = my_colors1[7]) +
    labs(x = "Log2 Carat" , y = "Log2 Price", title = "Diamonds Dataset") +
    ylim(8.2, 15.5)

##Now add in the smoothers
d + geom_line(aes(x = lcarat, y = lprice, color = smooth, linetype = smooth),
                data = . %>% filter(smooth != 'S'), linewidth = 0.8) +
    scale_colour_manual(values = my_colors2) +
    scale_linetype_manual(values = my_lines)
