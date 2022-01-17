#Bibliotheken
library(dplyr)


#Einlesen Daten
df_kleinbasel <- read.csv("kleinbasel.csv") 

# Berechnung ohne bisherige
df_kleinbasel_ohne <- filter(df_kleinbasel, bisher == F)
relation_kleinbasel_ohne <- lm(veraenderte_SPListen ~ rang, data=df_kleinbasel_ohne)
df_kleinbasel$korrekturwert <-  mean(predict(relation_kleinbasel_ohne,data.frame(rang = df_kleinbasel$rang))) - predict(relation_kleinbasel_ohne,data.frame(rang = df_kleinbasel$rang))
df_kleinbasel$neuberechnung <- df_kleinbasel$korrekturwert + df_kleinbasel$Total_Stimmen
df_kleinbasel <- arrange(df_kleinbasel,desc(Total_Stimmen))
df_kleinbasel$reihenfolge_alt <- c(1:27)

df_kleinbasel <- arrange(df_kleinbasel,desc(neuberechnung))
df_kleinbasel$reihenfolge_neu<- c(1:27)

#Resultate als .csv exportieren
resultate <- cbind(df_kleinbasel$name,df_kleinbasel$Total_Stimmen,df_kleinbasel$korrekturwert,df_kleinbasel$neuberechnung,df_kleinbasel$reihenfolge_alt,df_kleinbasel$reihenfolge_neu)
colnames(resultate) <- c("Name","Stimmen","Korrektur gemäss Modell","Stimmen korrigiert","Rang","Rang gemäss Modell")

write.csv(resultate,file="resultate.csv")