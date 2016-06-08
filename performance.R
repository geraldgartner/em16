#Loading necessary packages
library(rvest)
library(dplyr)
library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(lubridate)
library(reshape2)

#dStd.at-Style
theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))  


spielerdaten <- read.csv("~/Google Drive/dStd.at/em16/rskripte/performanceanalysis/spielerdaten.csv")
marktwerte <- read.csv("~/Google Drive/dStd.at/em16/rskripte/performanceanalysis/marktwerte.csv")

#tidying! all NAs to 0
spielerdaten[is.na(spielerdaten)] <- 0

#rescue all the dates
spielerdaten$gebdat <- as.Date(spielerdaten$gebdat,
                       origin = "1904-01-01")

today <- Sys.Date()
spielerdaten$age <- round((today-spielerdaten$gebdat)/365.25, digits = 1)

spielerdaten$age(start = gebdat, end = today) / 
  duration(num = 1, units = "years")

spielerdaten$einsatzquote <- as.numeric(spielerdaten$minutesplayed) / 1080
spielerdaten$torquote <- spielerdaten$tore / spielerdaten$minutesplayed


# Plotting: Sagt die Erfahrenheit der Spieler den Aufstieg voraus?
erfahrung <- ggplot(spielerdaten, aes(x=einsatzquote, y=laenderspiele, colour=q)) +
geom_point(alpha=1/4) + 
  geom_smooth(method=lm)  +
  scale_x_continuous(labels = percent) +
  labs(x = "Einsatzquote in der EM-Qualifikation", y = "Anzahl der Länderspiele insgesamt") +
  ggtitle("Je erfahrener die Teamspieler, \ndesto wahrscheinlicher die EM-Quali") +
  guides(fill=FALSE) +
  scale_colour_manual(values = c("nq"="#7A8FCC", "q"="#548750")) +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
  theme
plot(erfahrung)
ggsave("erfahrung.pdf", useDingbats=FALSE)

summary(lm(spielerdaten$einsatzquote + spielerdaten$laenderspiele ~ spielerdaten$q ))


test <- glm(spielerdaten$einsatzquote ~ spielerdaten$q, family = binomial)
print(test)

#Beste Torjäger
spielerdaten$torquote <- spielerdaten$tore/spielerdaten$matchesplayed
toremehrals0 <- subset(spielerdaten, tore>0)
torquotemehrals0 <- subset(spielerdaten, torquote>0)
torjäger <- data.frame(toremehrals0, torquotemehrals0)

torjägerplot <- ggplot(toremehrals0, aes(x=torquote, y=tore, colour=q)) +
  geom_point(alpha=1/4) + 
  labs(x = "Tore pro 90 Minuten", y = "Anzahl der Tore in der EM-Quali") +
  ggtitle("Je erfahrener die Teamspieler, \ndesto wahrscheinlicher die EM-Quali") +
  guides(fill=FALSE) +
  scale_x_log10() +
  scale_y_continuous() +
  scale_colour_manual(values = c("nq"="#7A8FCC", "q"="#548750")) +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
  theme +
  geom_text(data=subset(toremehrals0, tore > 8),
            aes(torquote,tore,label=spieler))
plot(torjägerplot)
ggsave("torjäger.pdf", useDingbats=FALSE)

ggplot(spielerdaten, aes(x=spielerdaten[torquote>"0"],
               y=spielerdaten[tore>"0"],
               colour=q))