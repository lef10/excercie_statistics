install.packages('questionr')
library(questionr)
library(dplyr)
library(tidyverse)

setwd("C:/Users/leandre.fabri/Documents/R/LF_cours/excercice_R")
getwd()

data(rp2018)

# modifier le tableau et cleaner en anglais

rp2018 <- rp2018 %>% 
  select(-pop_cl, -pop_0_14, -pop_15_29, -pop_18_24, -pop_75p, -pop_femmes, -pop_act_15p, -pop_agric, -pop_indep, -pop_interm, -pop_scol_18_24, 
         -pop_scol_18_24, -pop_non_scol_15p, -pop_dipl_bepc, -pop_dipl_bepc, -pop_dipl_capbep, -pop_dipl_bac, -pop_dipl_sup2,
         -pop_dipl_sup34, -log_rp, -log_proprio, -log_loc, -log_hlm, -log_sec, -log_maison, -log_appart, -age_0_14, -age_15_29,-age_75p,
         -femmes, -indep, -interm, -dipl_bepc, -dipl_capbep, -dipl_bac, -dipl_sup2, -dipl_sup2, -dipl_sup34, -resid_sec,
         -proprio, -hlm)

names(rp2018)[14] <- "unemployment" 
names(rp2018)[7] <- "pop_total" 
names(rp2018)[15] <- "farming" 
names(rp2018)[19] <- "student" 
names(rp2018)[8] <- "pop_unemployment" 
names(rp2018)[10] <- "pop_employment" 
names(rp2018)[17] <- "employment" 
names(rp2018)[16] <- "executive" 
names(rp2018)[9] <- "pop_executive" 
names(rp2018)[12] <- "pop_no_degree" 
names(rp2018)[20] <- "no_degree" 
names(rp2018)[21] <- "degree" 
names(rp2018)[13] <- "pop_degree" 
names(rp2018)[24] <- "home" 
names(rp2018)[22] <- "rent" 
names(rp2018)[18] <- "worker" 
names(rp2018)[11] <- "pop_worker" 


?rp2018

# ajouter la variable nord et sud

unique(rp2018$code_region)

rp2018 <- rp2018 %>% 
  mutate(caneva = case_when(
  code_region %in% c("32", "11", "24", "28", "44", "52", "53", "27") ~ "nord",
  code_region %in% c("75", "76", "93", "94", "84") ~ "sud",
  code_region %in% c("01", "02", "03", "04") ~ "dom",
  TRUE ~ "NA"
))

rp2018 <- rp2018 %>% 
  mutate(unemployment = case_when(
    departement == "Meurthe-et-Moselle" ~ NA,
    TRUE ~ rp2018$unemployment
  ))

#fichier de sortie
write.csv(rp2018, "output/rp2018.csv")

rp2018 <- rp2018 %>% 
  filter(caneva != "dom")

summary(rp2018$unemployment)
hist(rp2018$unemployment)
# distribution normal
hist(log(rp2018$unemployment))

g1 <- ggplot(rp2018, aes(x = log(unemployment))) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.5) + 
  theme_bw()
g1

########################## Test avec 2 échantillons ###########################

boxplot(formula = as.formula("unemployment~caneva"), data = rp2018)

#ggplot
g2 <- ggplot(rp2018, aes(x = caneva, y = log(unemployment), fill = caneva)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()
g2

t.test(x = rp2018$unemployment[rp2018$caneva == "sud"], 
       y = rp2018$unemployment[rp2018$caneva == "nord"]) # certaine incertitude quant à la présence d'une différence significative entre les taux de chômage dans les régions "Sud" et "Nord"

boxplot(formula = as.formula("unemployment~departement"), data = rp2018[rp2018$departement %in% c("Hérault", "Jura"),])

#ggplot graph 
g21 <- ggplot(rp2018, aes(x = departement, y = log(unemployment), fill = caneva)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()
g21

t.test(x = rp2018$unemployment[rp2018$departement == "Jura"], 
       y = rp2018$unemployment[rp2018$departement == "Hérault"]) # il y a une différence significative entre les taux de chômage dans les départements "Jura" et "Hérault"

# Shapiro-Wilks normality test pour les villes supérieurs à 60 000 habitants
# Le test de normalité de Shapiro-Wilk évalue si un échantillon de données suit une distribution normale.

qqnorm(y = log(rp2018$unemployment)[rp2018$pop_total > 100000 ])

shapiro.test(x = log(rp2018$unemployment)[rp2018$pop_total > 100000 ]) # l'échantillon suit une distribution normal avec un p value = 0.3032

#  p-value inférieur à 0.05 indique une différence statistiquement significative

plot(x = rp2018$executive[rp2018$caneva == "sud"], 
     y = rp2018$student[rp2018$caneva == "sud"],
     pch = 20
)

#### ggplot graph
g3 <- ggplot(rp2018, aes(x = executive, y = student, color = caneva)) + 
  geom_point(size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_classic()
g3

cor.test(x = rp2018$executive[rp2018$caneva == "sud"], 
         y = rp2018$student[rp2018$caneva == "sud"])

# il existe une corrélation statistiquement significative et positive entre les deux variables étudiées

cor.test(x = rp2018$student, y = rp2018$executive)

reg1 <- lm(student~executive, data = rp2018)
reg1
summary(reg1)

#tracer la droite de regression
g4 <-ggplot(rp2018, aes(x = executive, y = student, color = caneva)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(intercept = reg1$coefficients[1] ,
                  slope = reg1$coefficients[2])) +
  scale_color_brewer(palette = "Set1") +
  theme_classic()
g4

rp2018$caneva_fact <- factor(rp2018$caneva, levels = c("sud","nord", "dom"))

reg2 <- lm(unemployment~student+no_degree+degree+caneva_fact, data = rp2018)
summary(reg2)

g5 <- ggplot(rp2018, aes(x = caneva_fact , y = unemployment, col = caneva_fact))+
  geom_point()+
  scale_color_manual(values=c("green", "blue", "red"))
g5

#################### comparer les évolutions ############################

data(rp2012)

rp12.18 <- merge(rp2012, rp2018, by.x = "commune" , by.y = "commune", all.x	=	TRUE,	sort	=	FALSE)


# evolution du chomage 

rp12.18$chom.change <- rp12.18$unemployment.y - rp12.18$unemployment.x

mean(x = rp12.18$chom.change, na.rm = TRUE)





