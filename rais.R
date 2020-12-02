rais2018centroeste <- read.table("D:/Documents/arquivosAnalise/RAIS_2018/RAIS_VINC_PUB_CENTRO_OESTE.txt", 
                                 header = TRUE, sep = ';', stringsAsFactors = FALSE)
head(rais2018centroeste)
str(rais2018centroeste)
View(rais2018centroeste)


library(dplyr)
FabAcucar <- rais2018centroeste %>%
  filter(CNAE.2.0.Subclasse == 1071600 | 
           CNAE.2.0.Subclasse == 1072401)

View(FabAcucar)
FabAcucar$CBO.Ocupação.2002 <- as.integer(FabAcucar$CBO.Ocupação.2002)
TrabCultFab <- FabAcucar %>%
  filter(CBO.Ocupação.2002 == 622110)
View(TrabCultFab)

rais2018centroeste$CNAE.2.0.Classe <- as.integer((rais2018centroeste$CNAE.2.0.Classe))
AgrPecFlor <- rais2018centroeste %>%
  filter(CNAE.2.0.Classe == 1113 | 
           CNAE.2.0.Classe == 1121 | 
           CNAE.2.0.Classe == 1130 | 
           CNAE.2.0.Classe == 1148 | 
           CNAE.2.0.Classe == 1156 | 
           CNAE.2.0.Classe == 1164 | 
           CNAE.2.0.Classe == 1199 | 
           CNAE.2.0.Classe == 1211 | 
           CNAE.2.0.Classe == 1229 | 
           CNAE.2.0.Classe == 1318 | 
           CNAE.2.0.Classe == 1326 | 
           CNAE.2.0.Classe == 1334 | 
           CNAE.2.0.Classe == 1342 | 
           CNAE.2.0.Classe == 1351 | 
           CNAE.2.0.Classe == 1393 | 
           CNAE.2.0.Classe == 1415 | 
           CNAE.2.0.Classe == 1423 | 
           CNAE.2.0.Classe == 1512 | 
           CNAE.2.0.Classe == 1521 | 
           CNAE.2.0.Classe == 1539 | 
           CNAE.2.0.Classe == 1547 | 
           CNAE.2.0.Classe == 1555 | 
           CNAE.2.0.Classe == 1598 | 
           CNAE.2.0.Classe == 1610 | 
           CNAE.2.0.Classe == 1628 | 
           CNAE.2.0.Classe == 1636 | 
           CNAE.2.0.Classe == 1709 | 
           CNAE.2.0.Classe == 2101 | 
           CNAE.2.0.Classe == 2209 | 
           CNAE.2.0.Classe == 2306)

AgrPecFlor2 <- AgrPecFlor
library(stringr)
AgrPecFlor2$Vl.Rem.Janeiro.CC <- str_replace(AgrPecFlor2$Vl.Rem.Janeiro.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Fevereiro.CC <- str_replace(AgrPecFlor2$Vl.Rem.Fevereiro.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Março.CC <- str_replace(AgrPecFlor2$Vl.Rem.Março.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Abril.CC <- str_replace(AgrPecFlor2$Vl.Rem.Abril.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Maio.CC <- str_replace(AgrPecFlor2$Vl.Rem.Maio.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Junho.CC <- str_replace(AgrPecFlor2$Vl.Rem.Junho.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Julho.CC <- str_replace(AgrPecFlor2$Vl.Rem.Julho.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Agosto.CC <- str_replace(AgrPecFlor2$Vl.Rem.Agosto.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Setembro.CC <- str_replace(AgrPecFlor2$Vl.Rem.Setembro.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Outubro.CC <- str_replace(AgrPecFlor2$Vl.Rem.Outubro.CC, ",", ".")
AgrPecFlor2$Vl.Rem.Novembro.CC <- str_replace(AgrPecFlor2$Vl.Rem.Novembro.CC, ",", ".")
AgrPecFlor2$Vl.Salário.Contratual <- str_replace(AgrPecFlor2$Vl.Salário.Contratual, ",", ".")

AgrPecFlor2$Vl.Rem.Janeiro.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Janeiro.CC)
AgrPecFlor2$Vl.Rem.Fevereiro.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Fevereiro.CC)
AgrPecFlor2$Vl.Rem.Março.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Março.CC)
AgrPecFlor2$Vl.Rem.Abril.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Abril.CC)
AgrPecFlor2$Vl.Rem.Maio.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Maio.CC)
AgrPecFlor2$Vl.Rem.Junho.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Junho.CC)
AgrPecFlor2$Vl.Rem.Agosto.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Agosto.CC)
AgrPecFlor2$Vl.Rem.Setembro.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Setembro.CC)
AgrPecFlor2$Vl.Rem.Outubro.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Outubro.CC)
AgrPecFlor2$Vl.Rem.Novembro.CC <- as.numeric(AgrPecFlor2$Vl.Rem.Novembro.CC)
AgrPecFlor2$Vl.Salário.Contratual <- as.numeric(AgrPecFlor2$Vl.Salário.Contratual)

View(AgrPecFlor2)

hist(AgrPecFlor$Faixa.Hora.Contrat, breaks = 6,
     main = "Histograma das faixas de horas contratadas")

summary(AgrPecFlor$Qtd.Hora.Contr)

AgrPecFlor %>%
  group_by(Tipo.Vínculo) %>%
  summarise(total = n())

AgrPecFlor$Vl.Rem.Fevereiro.CC <- as.numeric (AgrPecFlor$Vl.Rem.Fevereiro.CC)

boxplot(data = AgrPecFlor2["Vl.Rem.Janeiro.CC", "Vl.Rem.Fevereiro.CC", "Vl.Rem.Março.CC", 
                          "Vl.Rem.Abril.CC", "Vl.Rem.Maio.CC", "Vl.Rem.Junho.CC",
                          "Vl.Rem.Julho.CC", "Vl.Rem.Agosto.CC", "Vl.Rem.Setembro.CC",
                          "Vl.Rem.Outubro.CC", "Vl.Rem.Novembro.CC", "Vl.Salário.Contratual"])

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Janeiro.CC), 
            minimo = min(Vl.Rem.Janeiro.CC),
            maximo = max(Vl.Rem.Janeiro.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Fevereiro.CC), 
            minimo = min(Vl.Rem.Fevereiro.CC),
            maximo = max(Vl.Rem.Fevereiro.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Março.CC), 
            minimo = min(Vl.Rem.Março.CC),
            maximo = max(Vl.Rem.Março.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Abril.CC), 
            minimo = min(Vl.Rem.Abril.CC),
            maximo = max(Vl.Rem.Abril.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Maio.CC), 
            minimo = min(Vl.Rem.Maio.CC),
            maximo = max(Vl.Rem.Maio.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Junho.CC), 
            minimo = min(Vl.Rem.Junho.CC),
            maximo = max(Vl.Rem.Junho.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Julho.CC), 
            minimo = min(Vl.Rem.Julho.CC),
            maximo = max(Vl.Rem.Julho.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Agosto.CC), 
            minimo = min(Vl.Rem.Agosto.CC),
            maximo = max(Vl.Rem.Agosto.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Setembro.CC), 
            minimo = min(Vl.Rem.Setembro.CC),
            maximo = max(Vl.Rem.Setembro.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Outubro.CC), 
            minimo = min(Vl.Rem.Outubro.CC),
            maximo = max(Vl.Rem.Outubro.CC),
            total = n())

AgrPecFlor2 %>%
  group_by(Tipo.Vínculo) %>%
  summarise(avg = mean(Vl.Rem.Novembro.CC), 
            minimo = min(Vl.Rem.Novembro.CC),
            maximo = max(Vl.Rem.Novembro.CC),
            total = n())

AgrPecFlor3 <- AgrPecFlor2 %>%
  group_by(Tipo.Vínculo)

View(AgrPecFlor3)
library(ggplot2)
ggplot(AgrPecFlor3, aes(x = as.factor(Tipo.Vínculo), y = Vl.Rem.Janeiro.CC)) + geom_boxplot()

mean(AgrPecFlor2$Vl.Rem.Janeiro.CC)
