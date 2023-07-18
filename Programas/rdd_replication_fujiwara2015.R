################### EXERCÍCIO DE REPLICAÇÃO: FUJIWARA (2015) ###################
######################### AUTOR: GUSTAVO HENRIQUE ##############################

#### Passo 1: Definir o Diretório de Trabalho ####
setwd("\\\\sbsb2/DISOC_RIO/BMT/RAIS/pisos/Projeto_Pisos2022/Replication_Fujiwara2015")

#### Passo 2: Importando os pacotes necessários ####
#if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",
               "haven",
               "estimatr",
               "stats",
               "fixest",
               "rdrobust",
               "rddensity",
               "rdd",
               "miceadds",
               "rddtools",
               "sf",
               "rnaturalearth",
               "rnaturalearthdata",
               "rgeos")

#### Passo 3: Carregando as bases ####
df_mun = read_dta('Dados/munic.dta')
# df_state = read_dta('Dados/state.dta')
# df_yearly = read_dta('Dados/yearly.dta')

#### Parte 4: Tratando as bases ####
df_mun = df_mun %>% 
  mutate(dep = voters96 - 40500,
         treat = ifelse(dep > 0, 1, 0),
         deptreat = (dep*treat),
         bw = ifelse(dep < 0, dep*-1, dep),
         ones = 1)
df_mun$bin_voters96 = cut(df_mun$voters96, breaks = c(seq(500, 200000, by = 4000)))
df_mun$bin_voters96 = as.numeric( sub("\\((.+),.*", "\\1", df_mun$bin_voters96))

df_mun = df_mun %>%
  mutate(bin_voters96 = bin_voters96 + 2000) %>% 
  group_by(bin_voters96) %>%
  mutate(bin_util94 = mean(r_util94, na.rm = T),
         bin_util98 = mean(r_util98),
         bin_util02 = mean(r_util02),
         bin_attend = mean(attend),
         bin_regist = mean(regist),
         bin_obs = sum(ones)) %>%
  ungroup()
# Taking the mean of Income lets us plot data roughly at the bin midpoints
ggplot(df_mun, aes(x = bin_voters96, y = bin_util98)) + 
  geom_line() + 
  # Add a cutoff line
  geom_vline(aes(xintercept = 40500), linetype = 'dashed')

#### Parte 5: Estimação ####
##### Realizando as estimações a nível de munícipio #####
rd1 = rdd_data(x = df_mun$dep, y = df_mun$r_util98, cutpoint = 0)
rdd_bw_ik(rd1, kernel = "Uniform")

reg2 = feols(c(r_util98, attend, regist, r_util94, r_util02) ~ treat + dep + deptreat,
      data = df_mun %>% 
        filter(bw < 10000),
      vcov = "HC1")
summary(reg2)
reg3 = feols(c(r_util98, attend, regist, r_util94, r_util02) ~ treat + dep + deptreat,
             data = df_mun %>% 
               filter(bw < 5000),
             vcov = "HC1")
summary(reg3)

# Filtrar os dados de acordo com as condições especificadas
filtered_data <- subset(df_mun, voters96 < 100000 & voters96 > 4500)

# Criar o gráfico
ggplot(filtered_data, aes(x = bin_voters96)) +
  geom_point(aes(y = bin_util94), color = "green", shape = 15, size = 2) +
  geom_point(aes(y = bin_util98), color = "blue", shape = 16, size = 2) +
  geom_point(aes(y = bin_util02), color = "red", shape = 17, size = 2) +
  geom_smooth(data = subset(df_mun, voters96 < 40500 & voters96 > 5000),
              aes(y = r_util94), color = "green", se = FALSE, method = "loess") +
  geom_smooth(data = subset(df_mun, voters96 < 100000 & voters96 > 40500),
              aes(y = r_util94), color = "green", se = FALSE) +
  geom_smooth(data = subset(df_mun, voters96 < 40500 & voters96 > 5000),
              aes(y = r_util02), color = "red", se = FALSE, method = "loess") +
  geom_smooth(data = subset(df_mun, voters96 < 100000 & voters96 > 40500),
              aes(y = r_util02), color = "red", se = FALSE) +
  geom_smooth(data = subset(df_mun, voters96 < 40500 & voters96 > 5000),
              aes(y = r_util98), color = "blue", se = FALSE, method = "loess") +
  geom_smooth(data = subset(df_mun, voters96 < 100000 & voters96 > 40500),
              aes(y = r_util98), color = "blue", se = FALSE) +
  xlab("Number of Registered Voters - 1996") +
  geom_vline(xintercept = 40500, color = "darkred") +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, by = 0.1), minor_breaks = NULL) +
  theme_bw()

# Criar o gráfico
ggplot(filtered_data, aes(x = bin_voters96)) +
  geom_point(aes(y = bin_regist), color = "green") +
  geom_point(aes(y = bin_attend), color = "blue", shape = 17) +
  geom_smooth(data = subset(df_mun, voters96 < 40500 & voters96 > 5000),
              aes(y = regist), color = "green", se = FALSE, method = "loess") +
  geom_smooth(data = subset(df_mun, voters96 < 100000 & voters96 > 40500),
              aes(y = regist), color = "green", se = FALSE) +
  geom_smooth(data = subset(df_mun, voters96 < 40500 & voters96 > 5000),
              aes(y = attend), color = "blue", se = FALSE, method = "loess") +
  geom_smooth(data = subset(df_mun, voters96 < 100000 & voters96 > 40500),
              aes(y = attend), color = "blue", se = FALSE) +
  xlab("Number of Registered Voters - 1996") +
  geom_vline(xintercept = 40500, color = "darkred") +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, by = 0.1), minor_breaks = NULL) +
  theme_bw()

