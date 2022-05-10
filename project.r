library(dplyr)
library(lubridate)
library(ggplot2)
library(olsrr)
library(zoo)
library(PerformanceAnalytics)
#load in cfc data
cfc <- read.csv("/cloud/project/ozone-depleting-substance-emissions.csv") %>%
  rename(tons_cfc11_equiv = Ozone.depleting.substance.emissions..Scientific.Assessment.2014.)
# filter cfc data
cfc <- cfc[cfc$Entity != "Natural emissions",]

#read in ozone data
ozone <- read.csv("/cloud/project/mfe-ozone-hole-19792016-CSV/ozone-hole-19792016.csv") %>%
  rename(ozone_area = Mean_maximum_ozone_hole_area, ozone_concentration = Mean_minimum_ozone_concentration)
#make combined data set,
# calculate ozone growth rates, 5 and 10 year averages, and squaed cfc emissions for analysis
combined <- merge(cfc, ozone, by = "Year")
combined$ozone_growth <- c(0, diff(combined$ozone_area))
combined$ozone_growth_avg5 <- rollmean(combined$ozone_growth, k = 5, fill = NA, align = "right")
combined$ozone_growth_avg10 <- rollmean(combined$ozone_growth, k = 10, fill = NA, align = "right")
combined$tons_cfc_sq <- combined$tons_cfc11_equiv ** 2

#calculate a quadratic model of cfc and 10 year ozone growht
model <- lm(ozone_growth_avg10 ~ 
              tons_cfc11_equiv+
              tons_cfc_sq, 
            data=combined)
model
summary(model)

#output results
write.csv(summary(model)$coefficients, "/cloud/project/reg.csv")

#check for ols assumptions
res.full <- rstandard(model)
fit.full <- fitted.values(model)

qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
shapiro.test(res.full)
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)


#graph 5 year ozone growth and emissions
coef <- .1
cfc_color <- "#000099"
ozone_color <- "#cc3300"
combined$hundred.thousand.tons.cfc <- combined$tons_cfc11_equiv/100000
ggplot(data = combined, aes(x = Year)) +
  geom_line(aes(y=hundred.thousand.tons.cfc), color = cfc_color) +
  geom_line(aes(y=ozone_growth_avg5/ coef), color = ozone_color)+
  geom_vline(xintercept=1987)+
  geom_text(aes(x=2005, label="Global CFC Ban exacted: 1987", y=22.5, family = "Times New Roman"), colour="black", 
            check_overlap = T, angle=0, vjust = 1.2, size = 4)+
  scale_y_continuous(
    name = "CFC emissions (hundred thousand tons)",
    sec.axis = sec_axis(~.*coef, name = expression(paste("Ozone Hole Growth (million K", m^{2}, " 5 year avg)"))))+
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(color = cfc_color, size=13),
    axis.title.y.right = element_text(color = ozone_color, size=13, hjust=.5),
    panel.background = element_blank(), 
    panel.grid.major = element_line(color = "grey"),
    text = element_text(family = "Times New Roman")
  ) +
  ggtitle("Ozone Hole vs CFC Emissions")
ozone_area_color= "#bb0032"

#graph ozone concentration and ozone hole area
coef2 <- .1
ggplot(data = combined, aes(x = Year))+
  geom_line(aes(y=ozone_concentration), color = cfc_color)+
  geom_line(aes(y=ozone_area/coef2), color = ozone_area_color)+
  scale_y_continuous(
    name = "Ozone Concentration (Dobson Units)",
    sec.axis = sec_axis(~.*coef2, name = expression(paste("Ozone Hole Area (million K",m^{2},")")))
    )+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(color = cfc_color, size=13),
        axis.title.y.right = element_text(color = ozone_area_color, size=13, hjust=.5),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey"),
        text = element_text(family = "Times New Roman")
  )+
  ggtitle("Tracking the Ozone Hole")

