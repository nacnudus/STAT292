require(ggplot2)
require(knitr)

dg.anova.oneway <- function(formula, data, levene = "squared"){
    #par(ask = TRUE)

    # Understand the model
    mf <- model.frame(formula, data)
    terms <- names(mf)
    response <- terms[1]
    independent <- terms[2]

    # Prepare an output object
    dg <- vector("list", 7)
    names(dg) <- c("data.raw", "boxplot", "lm" , "levene"
		   , "anova.table", "TukeyHSD" , "kruskal")

    # Spew the data
    dg[["data.raw"]] <- data

    # boxplot
    dg[["boxplot"]] <- ggplot(data, aes_string(x = independent, y = response)) +
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom = "point", colour = "red", shape = 5)
    print(dg[["boxplot"]])

    # residuals and qq (via a linear model)
    dg[["lm"]] <- lm(formula, data = data)
    dg.lm <- dg[["lm"]]
    plot(dg.lm, which = 1:2)

    # Levene's test (using squared residuals)
    dg.formula <- as.formula("residuals(dg.lm)^2 ~ data[, independent]")
    dg[["levene"]] <- anova(lm(dg.formula))
    kable(dg[["levene"]])

    # ANOVA table
    dg[["anova"]] <- anova(dg.lm)
    dg.anova <- dg[["anova"]]
    Total <- data.frame(sum(dg.anova[, 1]), sum(dg.anova[, 2])
    		    , NA, NA, NA, row.names = "Corrected Total")
    colnames(Total) <- colnames(dg.anova)
    dg[["anova"]] <- rbind(dg.anova, Total)
    kable(dg[["anova"]])

    # Tukey test
    dg[["TukeyHSD"]] <- TukeyHSD(aov(dg.lm))
    dg.tukey <- dg[["TukeyHSD"]]
    print(dg.tukey)
    plot(dg.tukey)

    # Kruskal-Wallis test
    dg[["kruskal"]] <- kruskal.test(formula, data = data)
    dg.kruskal <- dg[["kruskal"]]
    print(dg.kruskal)

    # Return the whole lot
    dg
}

dg.anova.twoway <- function(formula, data, levene = "squared"){
    #par(ask = TRUE)

    # Understand the model
    mf <- model.frame(formula, data)
    terms <- names(mf)
    response <- terms[1]
    independent1 <- terms[2]
    independent2 <- terms[3]

    # Prepare an output object
    dg <- vector("list", 4)
    names(dg) <- c("data.raw", "lm", "anova.table", "interaction.plot")

    # Spew the data
    dg[["data.raw"]] <- data

    # residuals and qq (via a linear model)
    dg[["lm"]] <- lm(formula, data = data)
    dg.lm <- dg[["lm"]]
    plot(dg.lm, which = 1:2)

    # ANOVA table
    dg[["anova"]] <- anova(dg.lm)
    dg.anova <- dg[["anova"]]
    Total <- data.frame(sum(dg.anova[, 1]), sum(dg.anova[, 2])
    		    , NA, NA, NA, row.names = "Corrected Total")
    colnames(Total) <- colnames(dg.anova)
    dg[["anova"]] <- rbind(dg.anova, Total)
    kable(dg[["anova"]])

    # Interaction plot
    dg[["interaction.plot"]] <- interaction.plot(data[, independent1],
						 data[, independent2],
						 data[, response])

    # Return the whole lot
    dg
}

dg.anova.threeway <- function(formula, data, levene = "squared"){
    #par(ask = TRUE)

    # Understand the model
    mf <- model.frame(formula, data)
    terms <- names(mf)
    response <- terms[1]
    independent1 <- terms[2]
    independent2 <- terms[3]
    independent3 <- terms[4]

    # Prepare an output object
    dg <- vector("list", 4)
    names(dg) <- c("data.raw", "lm", "anova.table", "interaction.plot")

    # Spew the data
    dg[["data.raw"]] <- data

    # residuals and qq (via a linear model)
    dg[["lm"]] <- lm(formula, data = data)
    dg.lm <- dg[["lm"]]
    plot(dg.lm, which = 1:2)

    # ANOVA table
    dg[["anova"]] <- anova(dg.lm)
    dg.anova <- dg[["anova"]]
    Total <- data.frame(sum(dg.anova[, 1]), sum(dg.anova[, 2])
    		    , NA, NA, NA, row.names = "Corrected Total")
    colnames(Total) <- colnames(dg.anova)
    dg[["anova"]] <- rbind(dg.anova, Total)
    kable(dg[["anova"]])

    # Return the whole lot
    dg
}
