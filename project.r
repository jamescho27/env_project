library(dplyr)
library(lubridate)
library(ggplot2)
library(olsrr)

cfc <- read.table("/cloud/project/HATS_global_F11.txt", header = TRUE, skip = 84)
ozone <- read.csv("/cloud/project/mfe-ozone-hole-19792016-CSV/ozone-hole-19792016.csv") %>%
  rename(ozone_area = Mean_maximum_ozone_hole_area, ozone_concentration = Mean_minimum_ozone_concentration)
cfc_year <- na.omit(cfc %>%
  group_by(HATS_F11_YYYY) %>%
  summarise_at(vars(HATS_Global_F11), list(CFC = mean))) %>% rename(Year = HATS_F11_YYYY)

combined <- merge(cfc_year, ozone, by = "Year")

model <- lm(ozone_area ~ CFC, data=combined)
model
summary(model)

coef <- .1
cfc_color <- "#000099"
ozone_color <- "#cc3300"
ggplot(data = combined, aes(x = Year)) +
  geom_line(aes(y=CFC), color = cfc_color) +
  geom_line(aes(y = ozone_area/ coef), color = ozone_color) +
  geom_vline(xintercept=1987, color = "black")+
  geom_vline(xintercept=2010, color = "black")+
  geom_text(aes(x=1987, label="CFC ban 1987", y=45), colour="black", angle=90, vjust = 1.2, size = 4)+
  geom_text(aes(x=2010, label="CFC11 ban 2010", y=45), colour="black", angle=90, vjust = 1.2, size = 4)+
  scale_y_continuous(
    name = "CFC11 parts per trillion",
    sec.axis = sec_axis(~.*coef, name = bquote("Ozone Hole Area Million"~Km^2)))+
  theme(legend.position='none') +
  theme(
    axis.title.y = element_text(color = cfc_color, size=13),
    axis.title.y.right = element_text(color = ozone_color, size=13)
  ) +
  ggtitle("Ozone Hole vs CFC Emissions")



