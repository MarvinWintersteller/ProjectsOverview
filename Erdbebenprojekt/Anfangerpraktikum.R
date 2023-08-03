library(tidyverse)
erdbeben3 <- readRDS("~/Desktop/erdbeben3.RDS")
library(stringr)
erdbeben3$ct <- as.numeric(str_remove_all(erdbeben4$`Crustal Thickness`, "\\."))
erdbeben3$mt <- as.numeric(str_remove_all(erdbeben4$`Mantle Thickness`, "\\."))
erdbeben3
skalierte_magnitude <- 0.002567*exp(2.558*(erdbeben3$Magnitude.x-3.9))
erdbeben3$Anzahl_direkter_Nachbeben_skaliert <- erdbeben3$`Anzahl direkter Nachbeben`
erdbeben3$Anzahl_direkter_Nachbeben_skaliert <- (erdbeben4$Anzahl_direkter_Nachbeben_skaliert)/skalierte_magnitude
erdbeben3 %>% select(`Anzahl direkter Nachbeben`, Anzahl_direkter_Nachbeben_skaliert)


# Mit mutate
erdbeben3 <- erdbeben3 %>% mutate(skalierte_magnitude = (0.002567*exp(2.558*(Magnitude.x-3.9))))
erdbeben3 <- erdbeben3 %>% mutate(Anzahl_direkter_Nachbeben_skaliert = (`Anzahl direkter Nachbeben`/skalierte_magnitude))
erdbeben3 %>% select(`Anzahl direkter Nachbeben`, Anzahl_direkter_Nachbeben_skaliert, skalierte_magnitude)


# Depth
#Vermutung: je tiefer die Tiefe ist, desto es meher Anzahl der direkte Nachbeben gibt.
boxplot(erdbeben3$`Depth(km).x`, xlab = "", ylab= "Depth(km).x")
# Boxplot von Depth: Maximum von der Tiefe ist 100. Minimum von der Tiefe ist 0.
mean(erdbeben3$`Depth(km).x`)
#der durchschnittliche Zahl liegt bei 36.9

plot(erdbeben3$`Depth(km).x`, erdbeben3$Anzahl_direkter_Nachbeben_skaliert, pch = 20, col = rgb(red = 0, blue = 1, green = 0, alpha = 0.2))

agg4 <- aggregate(erdbeben3$Anzahl_direkter_Nachbeben_skaliert, by = list(erdbeben3$`Depth(km).x`),mean)
#durchschnittliche Anzahl_direkter_Nachbeben_skaliert group by depth
agg4 <- agg4 %>% rename(Depth = Group.1, durchschnitt_Nachbeben_skaliert = x)
#Name aedern
agg4
plot(agg4$Depth, agg4$durchschnitt_Nachbeben_skaliert, pch = 20, col = rgb(red = 0, blue = 1, green = 0, alpha = 0.2))
# Anfang dachte ich, dass je tiefer ist, desto meher Anzahl der direkte Nachbeben gibt. Aber durch Plot kann man deutlich sehen, dass diese Aussage falsch ist.
max(erdbeben3$Anzahl_direkter_Nachbeben_skaliert)
1206.54
#Depth liegt bei kleiner als 20

cor(erdbeben3$`Depth(km).x`, erdbeben3$Anzahl_direkter_Nachbeben_skaliert)
#kleiner als 0, negative Korrelation 

library("ggpubr")
depthandnachbeben <- erdbeben3 %>% select(Anzahl_direkter_Nachbeben_skaliert,`Depth(km).x`)
depthandnachbeben
ggscatter(depthandnachbeben, x = "Depth(km).x", y = "Anzahl_direkter_Nachbeben_skaliert", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "depth", ylab = "Anzahl_direkter_Nachbeben_skaliert")



###Bezug zwischen Magnitude und Depth
#Vermutung: je tiefer die Tiefe ist, desto groesser Magnitude ist.
depthandmagnitude <- erdbeben3 %>% select(Magnitude.x,`Depth(km).x`)
depthandmagnitude
ggscatter(depthandmagnitude, x = "Depth(km).x", y = "Magnitude.x", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "depth", ylab = "Magnitude")
cor(erdbeben3$`Depth(km).x`,erdbeben3$Magnitude.x)
#kleiner als 0, negative Korrelation 
max(erdbeben3$Magnitude.x)
filter(erdbeben3,Magnitude.x == 8.7)
#bei maximum Magnitude ist Depth 23.74.
#Aussage: 



###Bezug zwischen (nicht) getriggert und Depth
# Beben, die selber nicht getriggert wurden
nicht_getriggert <- erdbeben3[erdbeben3$Triggerbeben == -1,]
nicht_getriggert
mean(nicht_getriggert$Magnitude.x)
#4.32559
mean(nicht_getriggert$`Depth(km).x`)
#39.90538


# Beben die getriggert worden sind
getriggert <- erdbeben3[erdbeben3$Triggerbeben > -1,]
mean(getriggert$Magnitude.x)
#4.408244
mean(getriggert$`Depth(km).x`)
#26.04664
#Beben, die selber nicht getriggert wurden, haben tiefer Depth als die Beben, die getriggert worden sind.
 






