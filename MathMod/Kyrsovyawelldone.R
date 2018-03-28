library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr")
tbl=read.csv("C:\\Users\\kiril\\Desktop\\Kireev_124\\eddypro.csv",skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#Преобразуем таблицу
tbl=tbl[-1,] 
tbl
#Из всего количества данных, выбираем те, которые связаны с летним периодом 
tbl=tbl[tbl$DOY > 152 & tbl$DOY < 244,] 
#ночные потоки
tbl=tbl[tbl$daytime == FALSE,]
glimpse(tbl) 
tbl = select(tbl, -(roll))
#Добавим новые столбцы, которые являются функциями существующих столбцов
tbl = tbl %>% mutate_if(is.character, factor)
#Меняем ненужные нам символы
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl)
#Берем каждую колонку таблицы, передаем ее в функию в виде вектора и выдаем результат в виде вектора длинной равной количеству колонок в таблице
sapply(tbl,is.numeric)
#Подставим этот вектор в саму таблицу и получм таблицу состояющую только из интересующих нас колонок
tbl_numeric = tbl [,sapply (tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux) 
cor_td 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
mod=lm(formula, data = tbl)
anova(mod)
summary(mod)
formula1 = as.formula(paste("h2o_flux ~ Tau + rand_err_Tau + LE + qc_LE + rand_err_LE + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es + RH + VPD + max_speed + u. + TKE + un_Tau + un_LE + un_h2o_flux + w_var + h2o_var"))
formula1
mod2=lm(formula1, data = tbl)
anova(mod2)
summary(mod2)
formula2 = as.formula(paste("h2o_flux ~ LE + qc_LE + RH + VPD + max_speed + u. + un_LE + un_h2o_flux + h2o_var"))
mod3=lm(formula2, data = tbl)
anova(mod3)
summary(mod3)
formula3 = as.formula(paste("h2o_flux ~ LE + RH + VPD + un_LE + un_h2o_flux + h2o_var"))
mod4=lm(formula3, data = tbl)
anova(mod4)
summary(mod4)
#взаимодействия переменных
mod5=lm(h2o_flux ~ (LE + RH + VPD + un_LE + un_h2o_flux + h2o_var) ^2, data = tbl)
mod5
anova(mod5)
summary(mod5)
mod6=lm(h2o_flux ~ LE + RH + un_LE + un_h2o_flux + LE:RH +LE:VPD + LE:h2o_var + RH:VPD+ VPD:un_LE + VPD:un_h2o_flux + un_LE:un_h2o_flux + un_LE:h2o_var + un_h2o_flux:h2o_var, data= tbl)                 
mod6      
anova(mod6)
summary(mod6)
mod7=lm(h2o_flux ~ LE + RH + un_LE + un_h2o_flux + LE:RH +LE:VPD + RH:VPD + VPD:un_LE +VPD:un_h2o_flux, data= tbl)  
anova(mod7)
summary(mod7)
