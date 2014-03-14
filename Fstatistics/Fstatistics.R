require(scales)                        # for log scales
require(ggplot2)

x <- expand.grid(c(0.95, 0.99), 1:10, 5:25)
names(x) <- c("significance", "df1", "df2")
x$Fstatistic <- qf(x$significance, x$df1, x$df2)

p <- ggplot(x, aes(df2, Fstatistic, colour = factor(df1))) + geom_point() + facet_wrap(~ significance)

