library(ggplot2)
library(tidyverse)
library(plotly)
library(ggplotlyExtra)


oildata<-read_csv("Tidy Data.csv")


oildata <- rename(oildata, Concentration = `Palm olein concentration(C)`, 
                  Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
oildata$Replicate<-as.factor(oildata$Replicate)


plot<-oildata %>% filter(Replicate==1)%>% filter(Series==c("Pure Palm Oil", "Pure VCO")) %>%
  ggplot(aes(x = W, y = A, color=Series)) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Wave Number (cm-1)", y = "Absorption")
ggplotly(plot)
