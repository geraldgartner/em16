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
library(ggrepel)


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
geom_point(alpha=1/3, aes(size = einsatzquote)) +
  scale_size_area(spielerdaten, max_size = 3) +
  geom_smooth(method=lm, aes(weight=einsatzquote))  +
  scale_x_continuous(labels = percent) +
  scale_fill_continuous(guide = "legend") +
  labs(x = "Einsatzquote in der EM-Qualifikation", y = "Anzahl der Länderspiele insgesamt") +
  ggtitle("Je erfahrener die Teamspieler, \ndesto wahrscheinlicher die EM-Quali") +
  scale_colour_manual(values = c("nq"="#CC7F7A", "q"="#7AAB89")) +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
  theme +
  geom_text_repel(data=subset(spielerdaten, laenderspiele > 100 & einsatzquote >0.7),
            aes(einsatzquote,laenderspiele,label=spieler))
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
  scale_colour_manual(values = c("nq"="#CC7F7A", "q"="#7AAB89")) +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
  theme +
  geom_text(data=subset(toremehrals0, tore > 8),
            aes(torquote,tore,label=spieler))
plot(torjägerplot)
ggsave("torjäger.pdf", useDingbats=FALSE)

ggplot(spielerdaten, aes(x=spielerdaten[torquote>"0"],
               y=spielerdaten[tore>"0"],
               colour=q))
###################################################################################################
###################################################################################################
#Sicherste Aufstellung

#Finde die 11 Spieler, die am häufigsten aufgestellt wurden
top11spieler <- tbl_df(spielerdaten) %>%
  group_by(land) %>%
  top_n(n = 11, wt = einsatzquote)

#Berechne Mean auf Länderbasis
top11spielerrefined <- aggregate( einsatzquote~land, top11spieler, mean )
#Sortiere absteigend
top11spielerrefined <- top13[order(top13$einsatzquote,decreasing=T),]


#Berechne Durchschnittsalter für die 11 am häufigsten aufgestellten
durchschnittsalter <- aggregate(age~land, top11spieler, mean )

#Merge dfs
sicherste11 <- merge(durchschnittsalter, top11spielerrefined, by.x = "land", by.y = "land")
sicherste12 <- merge(x = sicherste11, y = spielerdaten[ , c("land", "q")], by = "land")
sicherste13 <- unique(sicherste12)

sicherste13plot <- ggplot(sicherste13, aes(x=age, y=einsatzquote, colour=q)) +
  geom_point(alpha=1, aes(size = einsatzquote)) +
  scale_size_area(sicherste11, max_size = 3) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous() +
  scale_fill_continuous(guide = "legend") +
  scale_colour_manual(values = c("nq"="#CC7F7A", "q"="#7AAB89")) +
  geom_text(data=subset(sicherste13, einsatzquote > 0.0),
            aes(age,einsatzquote,label=land)) +
  coord_flip()+
  labs(x = "Durchschnittsalter der Spieler in der Mannschaft", y = "Einsatzquote aller Spieler") +
  ggtitle("Wales & Belgien: Junge \nerfolgshungrige Teams") +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
  theme
plot(sicherste13plot)
ggsave("sicherste13plot_confidence.pdf", useDingbats=FALSE)



###################################################################################################
###################################################################################################

#Torgefährlichste Spieler
spielerdaten$torquote <- spielerdaten$tore/spielerdaten$matchesplayed

spielerdaten$torschussquote <- (as.numeric(spielerdaten$attemptsontarget)+spielerdaten$attemptsofftarget)/spielerdaten$minutesplayed
toremehrals0 <- subset(spielerdaten, tore>0)
torschussquotemehrals0 <- subset(spielerdaten, torschussquote>0)
knipserdata <- merge(x = toremehrals0, y = torschussquotemehrals0[ , c("spieler", "torschussquote")], by = "spieler")


knipserplot <- ggplot(knipserdata, aes(x=torschussquote.x*15, y=tore, colour=q)) +
  geom_point(alpha=1/2) + 
  labs(x = "Schüsse aufs Tor pro Viertelstunde", y = "Anzahl der Tore in der EM-Quali") +
  ggtitle("Teams mit den \ngefährlichsten Torjägern") +
  geom_smooth(method=lm, aes(weight=torschussquote.x))  +
  scale_y_continuous() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = c("nq"="#CC7F7A", "q"="#7AAB89")) +
  theme +
  theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none")+
  geom_text(data=subset(knipserdata, tore > 6 & minutesplayed >120 & spieler=="Marc Janko" ),
            aes(torschussquote.x,tore,label=spieler))
plot(knipserplot)
ggsave("knipserplot.pdf", useDingbats=FALSE)
