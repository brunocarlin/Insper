library(tidyverse)
library(factoextra)
library(ggrepel)

(avaliacoes <- read_csv('advanced_predictive_modeling/data/avaliacoes.csv'))

(questoes <- read_csv('advanced_predictive_modeling/data/questoes.csv'))


avaliacoes %>% 
  count(marca) %>% 
  mutate(percentual = 100 * prop.table(n)) %>% 
  arrange(percentual %>% desc) %>% 
  ggplot() +
    aes(x = fct_reorder(marca,n), y = percentual) +
    geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') +
    xlab('Marcas') + ylab('Percentual das avaliacoes') +
    coord_flip()


avaliacoes %>% 
  group_by(respondente) %>% 
  tally()
  

avaliacoes %>% 
  group_by(respondente) %>% 
  summarise(num_marcas = n()) %>% 
  count(num_marcas) %>% 
  mutate(perc =  100 * prop.table(n)) %>% 
  ggplot(aes(x = num_marcas,y  = perc)) +
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') +
  xlab('Numero de marcas avaliadas') +
  ylab('percentual da amostra')

pca <- avaliacoes %>% 
  select(starts_with('Q')) %>% 
  prcomp(scale = TRUE)

# proportion of variance explained

(pve <- cumsum(pca$sdev ^2)/ sum(pca$sdev ^ 2))

fviz_eig(pca,addlabels = TRUE) +
  xlab('componete principal') +
  ylab('Proportion of explained variance')

Phi <- pca$rotation

pca %>% 
  fviz_contrib(choice = 'var', axes = 1, sort.val = 'asc', fill = 'steelblue', color = 'black') +
  labs(x = '', title = 'Contribuicoes das variaveis para o PC1') +
  coord_flip()


pca %>% 
  fviz_contrib(choice = 'var', axes = 2, sort.val = 'asc', fill = 'steelblue', color = 'black') +
  labs(x = '', title = 'Contribuicoes das variaveis para o PC2') +
  coord_flip()

pca %>% 
  fviz_contrib(choice = 'var', axes = 3, sort.val = 'asc', fill = 'steelblue', color = 'black') +
  labs(x = '', title = 'Contribuicoes das variaveis para o PC3') +
  coord_flip()

Z <- pca$x[, 1:3]
colnames(Z) <- sprintf('driver_%d',1:3)
colnames(Z)

get_driver <- function(Phi, questoes, drv, top) {
  tibble(numero = rownames(Phi),carga = Phi[, drv]) %>% 
    left_join(questoes) %>% 
    mutate(contribuicao = carga ^ 2/ sum(carga ^ 2)) %>% 
    arrange(contribuicao %>% desc) %>% 
    head(n = top)
}
driver_1 <- get_driver(Phi,questoes,drv = 1,top = 6)
Phi[,1] = - Phi[, 1]
Z[,1] = - Z[,1]

(driver_1 <- get_driver(Phi,questoes,drv = 1,top = 6))

(driver_2 <- get_driver(Phi,questoes,drv = 2,top = 10))

(driver_3 <- get_driver(Phi,questoes,drv = 3,top = 10))

tb <- tibble(marca = avaliacoes %>% pull(marca)) %>% 
  bind_cols(as_tibble(Z))

tb %>% 
  group_by(marca) %>% 
  summarise_all(mean) %>% 
  pivot_longer(names_to = 'driver',values_to = 'escore_medio',driver_1:driver_3) %>% 
  ggplot() +
  aes(x = driver, y = escore_medio,
      group = marca,
      color = marca,
      label = if_else(driver == 'driver_1', marca, '')) +
  geom_line(size = 1, alpha = 0.55) +
  labs(x = '', y = 'escore medio', title = 'posicionamento marcas') +
  geom_label_repel(direction = 'both') +
  scale_x_discrete(labels = c('limpeza','suavidade','intensidade'),expand = expand_scale(add = 0.05)) +
  theme(legend.position = 'none') +
  theme_bw()



  
