
# Produção de carne e laticínios em países democratas e autocratas -------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 04/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/meat-production -----------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Alimentar o mundo de uma forma sustentável é um dos nossos desafios mais prementes 
### nas próximas décadas. A carne desempenha um papel fulcral neste contexto.

### Carne é uma importante fonte de nutrição para muitas pessoas em todo mundo. A demanda
### global de carne está crescendo: passado 50 anos, a produção de carne tem mais que
### triplicado. O mundo agora produz mais que 340 milhões de toneladas por ano.

### Mas a produção de carne tem grandes impactos ambientais - aumentando gases de efeito
### estufa, terras de agricultura e uso de água. Um dos maiores desafios mundiais é
### produzir e consumir carne, laticínios e outros produtos protéicos de uma forma que
### reduza os impactos ambientais.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

prod_carne <- read.csv("meat-production-tonnes.csv")
view(prod_carne)
names(prod_carne)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

prod_carne <- prod_carne %>%
  select(-Code) %>%
  rename(producao = Meat..total...00001765....Production...005510....tonnes) %>%
  view()

prod_carne1 <- prod_carne %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(producao),
            sd = sd(producao), n = n(),
            se = sd/sqrt(n)) %>%
  view()

prod_carne2 <- prod_carne %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

prod_carne3 <- prod_carne %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(prod_carne1, aes(x = fct_reorder(Entity, media), 
                        y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Produção de carne (toneladas)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

