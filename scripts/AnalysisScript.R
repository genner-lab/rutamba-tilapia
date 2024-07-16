library(DHARMa)
library(car)

data <- read.csv("rutamba_growth_data.csv")
data$Species<- as.factor(data$Species)
data$Species<- relevel(data$Species, ref = "KOR")

lm1 <- lm(log10(Average_scaledist) ~ log10(Average_diameter) * Species, data = data)
lmer1X <- simulateResiduals(lm1)
plot(lmer1X)
car::Anova(lm1, type=c("II"))

lm2 <- lm(log10(Average_scaledist) ~ log10(SL) * Species, data = data)
lmer2X <- simulateResiduals(lm2)
plot(lmer2X)
car::Anova(lm2, type=c("II"))
