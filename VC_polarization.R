rm(list=ls()[!(ls() %in% c("cses_leg"))])
#rm(list=ls())


load("C:/Users/livia/Desktop/TESE_CSES/cses_leg.RData")

##### PACKAGES #####
library(dplyr)
library(stargazer)
library(ggplot2)
library(ggrepel)


##### CRIANDO OS DADOS ##### 


cses_leg$extreme <- abs(cses_leg$ideol_self - 5) # INDIVIDUAL EXTREMISM 

# Criar o "LH_all" para ideol_voted e exp_ideol_voted
cses_leg <- cses_leg %>% mutate (
  ideol_voted_LH_all = case_when(
    is.na(ideol_voted_LH_DC) ~ ideol_voted_LH_PL,
    is.na(ideol_voted_LH_PL) ~ ideol_voted_LH_DC,
    !is.na(ideol_voted_LH_DC) && !is.na(ideol_voted_LH_PL) ~ (ideol_voted_LH_DC+ideol_voted_LH_PL)/2
  )
)

cses_leg <- cses_leg %>% mutate (
  exp_ideol_voted_LH_all = case_when(
    is.na(exp_ideol_voted_LH_DC) ~ exp_ideol_voted_LH_PL,
    is.na(exp_ideol_voted_LH_PL) ~ exp_ideol_voted_LH_DC,
    !is.na(exp_ideol_voted_LH_DC) && !is.na(exp_ideol_voted_LH_PL) ~ (exp_ideol_voted_LH_DC+exp_ideol_voted_LH_PL)/2
  )
)

#Party voted polarization :

cses_leg$extreme_voted <- abs(cses_leg$ideol_voted_LH_all - 5)
cses_leg$extreme_expvoted <- abs(cses_leg$exp_ideol_voted_LH_all - 5)

##### POLARIZATION OF VOTERS #####

cses_leg$voter_extreme <- cses_leg$extreme
cses_leg$voter_extreme[cses_leg$voted_LH == 0] <- NA

#POLARIZATION OF NON-VOTERS

cses_leg$abs_extreme <- cses_leg$extreme
cses_leg$abs_extreme[cses_leg$voted_LH == 1] <- NA


##### AGRUPANDO #####
grouped_leg <- cses_leg %>% select (country, election, contains("extreme"), 
                                    dalton_pol, compulsory_dummy, 
                                    compulsory, ENEP, gallagher,
                                    cyear, ideol_self, 
                                    exp_ideol_voted_LH_all,
                                    ideol_voted_LH_all, education, knowledge_adj, 
                                    size_LH, freedom_house_1) %>% 
  group_by (country, election, cyear) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names(grouped_leg) <- gsub("_mean", "", names(grouped_leg))


##### QOG DATA #####

qog<- read.csv("C:/Users/livia/Desktop/TESE_CSES/qog_std_ts_jan20.csv")

qog <- qog %>% filter (year > 1995) %>% 
  select (year, ccodealp, cname, wdi_gini, al_ethnic2000, al_religion2000,
          al_language2000)

qog$cyear <- paste(qog$ccodealp, qog$year) 
qog$cyear <- gsub(" ", "_", qog$cyear)


grouped_leg$gini <- qog$wdi_gini[match(grouped_leg$cyear, qog$cyear)]
grouped_leg$ethnic <- qog$al_ethnic2000[match(grouped_leg$cyear, qog$cyear)]
grouped_leg$religion <- qog$al_religion2000[match(grouped_leg$cyear, qog$cyear)]
grouped_leg$language <- qog$al_language2000[match(grouped_leg$cyear, qog$cyear)]

##### BOXPLOTS #####

#Little changes in data 
grouped_leg$compulsory_dummy <- as.factor(grouped_leg$compulsory_dummy)
grouped_leg$dalton_pol <- grouped_leg$dalton_pol/2 #Só para ficar igual os outros


ggplot(grouped_leg, aes(x=extreme, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none" , axis.title.y = element_blank()) + 
  labs (x="All citizen") +
  stat_summary(fun.y=mean, geom="point", size=2) 

ggplot(grouped_leg, aes(x=voter_extreme, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none" , axis.title.y = element_blank()) + 
  labs (x="Voters") +
  stat_summary(fun.y=mean, geom="point", size=2) 

ggplot(grouped_leg, aes(x=extreme_voted, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none", axis.title.y = element_blank() 
  ) + labs (x="Party voted (citizen score)") +
  stat_summary(fun.y=mean, geom="point", size=2)

ggplot(grouped_leg, aes(x=extreme_expvoted, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none", axis.title.y = element_blank()  
  ) + labs (x="Party voted (expert score)") +
  stat_summary(fun.y=mean, geom="point", size=2)

ggplot(grouped_leg, aes(x=dalton_pol, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none", axis.title.y = element_blank()  
  ) + labs (x="Dalton index") +
 stat_summary(fun.y=mean, geom="point", size=2)



p + stat_summary(fun.y=mean, geom="point", size=2, color="red")

# Aqui temos uma clara polarização maior entre PARTIDOS para VOTO FACULTATIVO, e também claramente maior do que 
#nas outras medidas utilizadas. 

##### REGRESSÕES BIVARIADAS #####

b1  <- glm(extreme ~ compulsory_dummy, data = grouped_leg) 

b2  <- glm(extreme ~ compulsory_dummy, data = grouped_vot) 

b3  <- glm(extreme_voted ~ compulsory_dummy, data = grouped_vot) 

b4  <- glm(extreme_expvoted ~ compulsory_dummy, data = grouped_vot) 

b5  <- glm(dalton_pol ~ compulsory_dummy, data = grouped_leg) 


##### REGRESSÕES MULTIVARIADAS #####

# PRESIDENTIALISM, RP (binária? por enquanto com Gallagher!)
#Religious, language fractionalization?

m1  <- glm(extreme ~ compulsory_dummy + gallagher + ENEP + gini +
             ethnic, data = grouped_leg) 

m2  <- glm(extreme ~ compulsory_dummy + gallagher + ENEP + gini +
              ethnic, data = grouped_vot) 

m3  <- glm(extreme_voted ~ compulsory_dummy + gallagher + ENEP + gini +
             ethnic, data = grouped_vot) 

m4  <- glm(extreme_expvoted ~ compulsory_dummy + gallagher + ENEP + gini +
             ethnic, data = grouped_vot) 

m5  <- glm(dalton_pol ~ compulsory_dummy + gallagher + ENEP + gini +
                ethnic, data = grouped_leg) 


stargazer(b1,m1,b2,m2) # Citizen e voter, o problema é que fica ruim de ver - dá pra mudar o 
#1,2,3,4 por "citizen, etc." mas é melhor então a ordem !


stargazer(b3,b4,b5)

stargazer(m3,m4,m5)


######  MODELOS SEM GINI #####

g1 <- glm(extreme ~ compulsory_dummy + gallagher + ENEP +
             ethnic, data = grouped_leg) 

g2 <- glm(extreme ~ compulsory_dummy + gallagher + ENEP + 
             ethnic, data = grouped_vot) 

g3 <- glm(extreme_voted ~ compulsory_dummy + gallagher + ENEP + 
             ethnic, data = grouped_vot) 

g4 <- glm(extreme_expvoted ~ compulsory_dummy + gallagher + ENEP + 
             ethnic, data = grouped_vot) 

g5 <- glm(dalton_pol ~ compulsory_dummy + gallagher + ENEP + 
             ethnic, data = grouped_leg) 

stargazer(g1,g2,g3,g4,g5)

##### PLOTS #####

# color = "#0099f9"

grouped_leg$compuls_cat <- "No"
grouped_leg$compuls_cat[grouped_leg$compulsory_dummy == 1] <- "Yes"

ggplot(grouped_leg, aes(x = dalton_pol, y = extreme, color = compuls_cat)) + 
  geom_point(size = 1.6) +
  labs( x = "Parliament (Dalton index)",
    y = "Citizen",
    color = "Compulsory vote"
  ) + theme(legend.position="bottom", 
            axis.title = element_text(size = 9),
            legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 8)) +
  scale_color_manual(values = c("darkolivegreen4","deepskyblue3"))



ggplot(grouped_leg, aes(x = voter_extreme, y = extreme, color = compuls_cat)) + 
  geom_point(size = 1.6) +
  labs( x = "Voters only",
        y = "All citizen",
        color = "Compulsory vote"
  ) + theme(legend.position="bottom", 
            axis.title = element_text(size = 9),
            legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 8)) +
  scale_color_manual(values = c("darkolivegreen4","deepskyblue3"))



ggplot(grouped_leg, aes(x = voter_extreme, y = abs_extreme, color = compuls_cat)) + 
  geom_point(size = 1) + geom_smooth(method = "lm", se = FALSE)+ 
  labs( x = "Voters",
        y = "Absent",
        color = "Compulsory vote"
  ) + theme(legend.position="bottom", 
            axis.title = element_text(size = 9),
            legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 8)) +
  scale_color_manual(values = c("darkolivegreen4","blue"))


###### FUTURE IMPROVEMENTS #####

#Balancear eleitores e não eleitores (só achar uma variável para turnout e multiplicar
# turnout * extreme if voted ==1, 1-turnout * extreme se for não eleitor
#aí tirar a média normal. 


