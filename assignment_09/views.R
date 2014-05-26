views <- read.csv("views.csv")
views <- views[rep(rownames(views), views$count), 1:3]
views <- data.frame(lapply(views, factor))
views$view <- relevel(views$view, 7)
views$race <- relevel(views$race, 2)
views$vote <- relevel(views$vote, 2)

views.lm <- glm(vote ~ view + race, data = views, family = "binomial")
summary(views.lm)

views.odds.race <- exp(coef(views.lm)[8])
views.confidence.vote <- exp(confint.default(views.lm)[8, ])
exp(confint(views.lm, level = 0.975)[8, ])
exp(confint.default(views.lm, "race1"))
