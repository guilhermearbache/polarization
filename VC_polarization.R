

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


cses_leg$extreme_voted <- abs(cses_leg$ideol_voted_LH_all - 5)

cses_leg$extreme_expvoted <- abs(cses_leg$exp_ideol_voted_LH_all - 5)


##### AGRUPANDO #####
grouped_leg <- cses_leg %>% select (country, election,cyear, ideol_self, exp_ideol_voted_LH_all,
                                    ideol_voted_LH_all, starts_with("extreme"), compulsory,
                                    compulsory_dummy, dalton_pol, ENEP,
                                    gallagher, education, knowledge_adj, 
                                    size_LH, freedom_house_1) %>% group_by (country, election, cyear) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names(grouped_leg) <- gsub("_mean", "", names(grouped_leg))


##### FILTERING ONLY VOTERS #####

cses_voters <- cses_leg %>% filter (voted_LH == 1) #FILTRAR ELEITORES


#MÉDIA - SUMMARIZE

grouped_vot <- cses_voters %>% select (country, election, cyear, ideol_self, exp_ideol_voted_LH_all,
                                       ideol_voted_LH_all, starts_with("extreme"), compulsory,
                                       compulsory_dummy, dalton_pol, ENEP,
                                       gallagher, education, knowledge_adj, 
                                       size_LH, freedom_house_1) %>% group_by (country, election, cyear) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names(grouped_vot) <- gsub("_mean", "", names(grouped_vot))


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

grouped_vot$gini <- qog$wdi_gini[match(grouped_vot$cyear, qog$cyear)]
grouped_vot$ethnic <- qog$al_ethnic2000[match(grouped_vot$cyear, qog$cyear)]
grouped_vot$religion <- qog$al_religion2000[match(grouped_vot$cyear, qog$cyear)]
grouped_vot$language <- qog$al_language2000[match(grouped_vot$cyear, qog$cyear)]


##### GRÁFICOS #####

ggplot(data=grouped_leg, aes(x=extreme, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_histogram() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distância ideológica mínima por eleitor")

#VOTO: 

ggplot(data=grouped_vot, aes(x=extreme_voted, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização - voto")

ggplot(data=grouped_vot, aes(x=extreme_expvoted, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização - voto (experts)")

#Maior polarização em países com compulsório, mais uniforme entre eles mas muitos casos que ficam próximos de 3, 
#facultativo tem pico mais forte perto de 2


#Polarização do voto e dos ELEITORES tendência parecida, mas de VOTO - EXPERTS não, aí fica muito próximo compulsório e facultativo.

# ELEITORES (IDEOLOGIA PESSOAL) 

ggplot(data=grouped_vot, aes(x=extreme, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização entre eleitores")

# DALTON 

ggplot(data=grouped_vot, aes(x=dalton_pol, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização entre eleitores")

##### BOXPLOTS #####

grouped_vot$compulsory_dummy <- as.factor(grouped_vot$compulsory_dummy)
grouped_leg$compulsory_dummy <- as.factor(grouped_leg$compulsory_dummy)

grouped_leg$dalton_pol <- grouped_leg$dalton_pol/2


ggplot(grouped_leg, aes(x=extreme, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none" , axis.title.y = element_blank()) + 
  labs (x="All citizen") +
  stat_summary(fun.y=mean, geom="point", size=2) 

ggplot(grouped_vot, aes(x=extreme, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none" , axis.title.y = element_blank()) + 
   labs (x="Voters") +
  stat_summary(fun.y=mean, geom="point", size=2)

ggplot(grouped_vot, aes(x=extreme_voted, y=compulsory_dummy,  color = compulsory_dummy)) + 
  geom_boxplot() + theme(legend.position="none", axis.title.y = element_blank() 
  ) + labs (x="Party voted (citizen score)") +
  stat_summary(fun.y=mean, geom="point", size=2)

ggplot(grouped_vot, aes(x=extreme_expvoted, y=compulsory_dummy,  color = compulsory_dummy)) + 
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


stargazer(b3,m3,b4,m4,b5)


m1  <- glm(extreme ~ compulsory_dummy + gini, data = grouped_leg) 

ggplot(grouped_leg, aes(x = dalton_pol, y = extreme)) + 
  geom_point(size = 2.5, color = "#0099f9") +
  geom_text_repel(label = grouped_leg$election,  size=1.5) +  labs(
    x = "Dalton",
    y = "Citizen"
  )



###### FUTURE IMPROVEMENTS #####

#JUNTAR NO MESMO DATASET OS DADOS PARA ELEITORES APENAS - FAZER O FILTRO ANTES DE SUMMARIZE
#TALVEZ NEM PRECISE FILTRAR, SÓ CRIAR UMA NOVA VERSÃO DE "EXTREME" COM NA PARA QUEM NÃO VOTOU


