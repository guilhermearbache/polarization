

#rm(list=ls()[!(ls() %in% c("cses_leg"))])
#rm(list=ls())


load("C:/Users/livia/Desktop/TESE_CSES/cses_leg.RData")

##### PACKAGES #####
library(dplyr)
library(ggplot2)

# CRIANDO OS DADOS 

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
grouped_leg <- cses_leg %>% select (country, election, ideol_self, exp_ideol_voted_LH_all,
                                    ideol_voted_LH_all, starts_with("extreme"), compulsory,
                                    compulsory_dummy, dalton_pol, 
                                    gallagher, education, knowledge_adj, 
                                    size_LH, freedom_house_1) %>% group_by (country, election) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names(grouped_leg) <- gsub("_mean", "", names(grouped_leg))


ggplot(data=grouped_leg, aes(x=extreme, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_histogram() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distância ideológica mínima por eleitor")



ggplot(data=grouped_leg, aes(x=extreme)) +
  geom_density() +
 facet_wrap(~compulsory_dummy) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distância ideológica mínima por eleitor")


#VOTO: 

ggplot(data=grouped_leg, aes(x=extreme_voted, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização - voto")

ggplot(data=grouped_leg, aes(x=extreme_expvoted, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização - voto (experts)")

#Maior polarização em países com compulsório, mais uniforme entre eles mas muitos casos que ficam próximos de 3, 
#facultativo tem pico mais forte perto de 2


#Polarização do voto e dos ELEITORES tendência parecida, mas de VOTO - EXPERTS não, aí fica muito próximo compulsório e facultativo.

## COMO APRESENTAR ISSO MELHOR? 


##### NÍVEL MÉDIO DE DISPERSÃO POR PAÍS (VOTERS) #####

cses_voters <- cses_leg %>% filter (voted_LH == 1) #FILTRAR ELEITORES


#MÉDIA - SUMMARIZE

grouped_vot <- cses_voters %>% select (country, election, ideol_self, extreme, compulsory,
                                    compulsory_dummy, dalton_pol, 
                                    gallagher, education, knowledge_adj, 
                                    size_LH, freedom_house_1) %>% group_by (country, election) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names(grouped_vot) <- gsub("_mean", "", names(grouped_vot))


ggplot(data=grouped_vot, aes(x=extreme, group=compulsory_dummy, color=compulsory_dummy)) +
  geom_density() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Polarização entre eleitores")




##### NÍVEL MÉDIO DE DISPERSÃO POR PAÍS - PARLIAMENT 


##### DALTON 



# REGRESSÃO BIVARIADA, INSERIR OUTRAS VARIÁVEIS 