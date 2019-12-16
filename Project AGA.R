#Invocamos los packages que vamos a utilizar.
#devtools::install_github('cttobin/ggthemr')
library(ggthemes)
library(gapminder)
library(rio)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(forcats)
library(ggthemr)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(knitr)
ggthemr("dust")

#Importamos los datos. (data_wide son los datos sobre cultivos, data_wide2 son los datos sobre ramaderia y world son los datos geospaciales. Además, estos últimos a parte de importarlos los ordenamos y nos los quedamos con el formato que nos interesa.)

data_wide <- import("./Bases de Dades/FAOSTAT/Cultius/Production_NOFLAG.csv")
#data_wide2 <- import("./Bases de Dades/FAOSTAT/Ramaderia/Production_LivestockPrimary_E_All_Data_NOFLAG.csv")
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(subunit , geometry) %>% filter(subunit != c("Antarctica" , "Greenland"))

#Arreglamos los datos para obtener 3 data frames (y tres más con datos geográficos) que nos interesan: uno de la superficie cultivada, otro de la producción y otro del rendimiento de la superficie.
data_wide_g <- left_join(world, data_wide, by = c("subunit" = "Area"))
data_wide_g <- data_wide_g %>% rename(Area = subunit)

df_ah <- data_wide %>% filter(Element == "Area harvested") %>%
    gather(year, area_harvested, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year)
df_ah_year <- as.numeric(df_ah$year_rem)
df_ah$year_rem = NULL
df_ah$rem = NULL
df_ah <- df_ah %>%
    add_column("year" = df_ah_year)

df_ah_g <- data_wide_g %>% filter(Element == "Area harvested") %>%
    gather(year, area_harvested, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year) %>%
  filter(area_code < 5000)
df_ah_g_year <- as.numeric(df_ah_g$year_rem)
df_ah_g$year_rem = NULL
df_ah_g$rem = NULL
df_ah_g <- df_ah_g %>%
  add_column("year" = df_ah_g_year)

#df_ah_g_pob <- left_join(gapminder , df_ah_g , by = c("country" = "area" , "year" = "year"))
#df_ah_g_pob <- rename(.data = df_ah_g_pob , area = "country")

df_y <- data_wide %>% filter(Element == "Yield") %>%
    gather(year, yield, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year)
df_y_year <- as.numeric(df_y$year_rem)
df_y$year_rem = NULL
df_y$rem = NULL
df_y <- df_y %>%
    add_column("year" = df_y_year)

df_y_g <- data_wide_g %>% filter(Element == "Yield") %>%
    gather(year, yield, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year)
df_y_g_year <- as.numeric(df_y_g$year_rem)
df_y_g$year_rem = NULL
df_y_g$rem = NULL
df_y_g <- df_y_g %>%
    add_column("year" = df_y_g_year)

df_p <- data_wide %>% filter(Element == "Production") %>%
    gather(year, production, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year)
df_p_year <- as.numeric(df_p$year_rem)
df_p$year_rem = NULL
df_p$rem = NULL
df_p <- df_p %>%
    add_column("year" = df_p_year)
df_p_g <- data_wide_g %>% filter(Element == "Production") %>%
    gather(year, production, 8:64) %>%
    separate(year, c("rem", "year"), sep = "Y") %>%
    rename(area_code = "Area Code" , area = Area , item_code = "Item Code" , item = Item , element_code = "Element Code" , element = Element , unit = Unit , year_rem = year)
df_p_g_year <- as.numeric(df_p_g$year_rem)
df_p_g$year_rem = NULL
df_p_g$rem = NULL
df_p_g <- df_p_g %>%
    add_column("year" = df_p_g_year)

#Eliminamos los df que no nos interesan y nos quedamos con los tres que sí.

rm(data_wide , data_wide_g , data_wide2 , df_ah_year , df_ah_g_year , df_p_year , df_p_g_year , df_y_year , df_y_g_year)

#Hem de conèixer les nostres dades: el area_code<5000 és per païssos, area_code=5000 és el World, area_code>5000 és per continents (centenes) i per regions dins dels continents (mateixa centena del continent i diferent desena).

#Además, como podemos ver, tenemos 180 items de alimentos. Los alimentos acon código >1000 son sumatorios y cantidades agregadas.

#zz <- df_p %>% select(production , item , item_code) %>%
 #  filter(item_code > 1000) %>%
  #  group_by(item) %>%
   # summarise(prod_it = sum(production , na.rm = TRUE))

#1. ¿Cuál es la producción agraria a nivel global?

#Si entenem la suma de tots els items com a la producció d'aliment global aleshores aquesta gràfica és correcta, però hem de tindre en compte que ací en tenim la producció en tones de l'agricultura, no de la ramaderia o la pesca. També hem de tindre en compte que no és el mateix una tona de blat que una tona d'atmetles, per tant, podria ser que l'evolució ascendent de les tones produides per l'agricultura vinguera produida per un increment en el pes dels aliments i no en un major valor afegit. (gràfic poc revelador per conèixerl'aport de l'agricultura al PIB)

aa1 <- df_p %>% select(area , item_code , year , production) %>%
    filter(area == "World") %>%
    filter(item_code < 1000) %>%
    group_by(year) %>%
    summarise(prod_tot_anual = sum(production*100/2566008375 , na.rm = TRUE))

aa2 <- df_ah %>% select(area , item_code , year , area_harvested) %>%
    filter(area == "World") %>%
    filter(item_code < 1000) %>%
    group_by(year) %>%
    summarise(ah_tot_anual = sum(area_harvested*100/971294535 , na.rm = TRUE))


ggplot(aa1 , aes(x = year)) +
    geom_line(mapping = aes(y = prod_tot_anual , color = prod_tot_anual)) +
  geom_line(data = aa2 , mapping = aes(y = ah_tot_anual , color = ah_tot_anual)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1 , name = "Area Cultivada")) +
  labs(title = "PRODUCCIÓN AGRÍCOLA MUNDIAL" , subtitle = "Evolución anual (de 1961 a 2017) dinámica de la producción y area cultivada agregada" , caption = "En números ídice 100 = Año 1961" , x = "Años" , y = "Producción Anual") +
    scale_colour_gradientn(colours = rainbow(4)) +
    transition_reveal(year) +
    ease_aes('linear') +
  theme(legend.position="none") +
  annotate(geom = "text", x = 1980, y = 100, label = "Area", size = 5) +
  annotate(geom = "text", x = 1980, y = 200, label = "Producción", size = 5)

#Elegimos los 8 alimentos con más producción en 2017 (en tones) en todo el mundo (intuim que són els més consumits) y vemos en qué regiones del mundo se producen.

bb <- df_p %>% select(item_code , item , area_code , area , year , production) %>%
    filter(area_code == 5000) %>%
    filter(year == 2017) %>%
    filter(item_code < 1000) %>%
    group_by(item , item_code) %>%
    summarise(prod_tot_item = sum(production , na.rm = TRUE)) %>%
    arrange(desc(prod_tot_item)) %>%
    ungroup(item_code , item) %>%
    top_n(6) %>%
    mutate(item = as_factor(item))

top_item <- bb$item
top_item_code <- bb$item_code

ggplot(data = bb , aes(x = item , y = prod_tot_item/1000000000 , fill = item)) +
    geom_col() +
  labs(title = "PRODUCTOS AGRÍCOLAS MÁS PRODUCIDOS" , subtitle = "De mayor a menor, las respectivas cantidades producidas en el año 2017" , caption = "En miles de millones de toneladas" , x = "Producto agrícola" , y = "Producción Anual") +
    scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none")

cc <- df_p %>% select(item_code , item , area_code , area , year , production) %>%
    filter(area_code %in% c(5100 , 5200 , 5300 , 5400 , 5500)) %>%
    filter(year == 2017) %>%
    filter(item_code %in% top_item_code) %>%
    group_by(area_code , area , item_code , item) %>%
    summarise(prod_reg = sum(production , na.rm = TRUE)) %>%
    ungroup(area_code , area , item_code , item) %>%
    select(area , item , prod_reg)

ggplot(data = cc , aes(x = area , y = prod_reg/1000000 , fill = area)) +
    geom_col() +
    facet_wrap(vars(item) , scales = "free") +
  labs(title = "PRODUCCIÓN POR CONTINENTE" , subtitle = "Las respectivas cantidades producidas en el año 2017" , caption = "En millones de toneladas" , x = "Continente" , y = "Producción Anual") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1) , legend.position ="none")

#Esta es la producción agregada, ¿pero quién es el líder en rendimiento de la tierra?

dd <- df_y %>% select(item_code , item , area_code , area , year , yield) %>%
    filter(area_code %in% c(5100 , 5200 , 5300 , 5400 , 5500)) %>%
    filter(item_code %in% top_item_code) %>%
    filter(year %in% c(1967 , 1977 , 1987 , 1997 , 2007 , 2017)) %>%
    group_by(area_code , area , item_code , item , year) %>%
    summarise(y_reg = sum(yield , na.rm = TRUE)) %>%
    ungroup(area_code , area , item_code , item , year) %>%
    select(area , item , y_reg , year)

ggplot(data = dd , aes(x = area , y = y_reg/10000 , fill = area)) +
    geom_col() +
    facet_wrap(vars(item) , scales = "free") +
    transition_states(year , transition_length = 10 , state_length = 40) +
    ease_aes('sine-in-out') +
  labs(title = "EFICIENCIA AÑO: {closest_state}" , subtitle = "Las respectivas eficiencias de la tierra del 1967 al 2017" , caption = "En toneladas por hectárea" , x = "Continente" , y = "Eficiencia") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1) , legend.position = "none")



ee <- inner_join(df_ah , df_p , by = c("area_code" = "area_code" , "area" = "area" , "item_code" = "item_code" , "item" = "item" , "year" = "year"))
ee <- inner_join(df_y , ee ,  by = c("area_code" = "area_code" , "area" = "area" , "item_code" = "item_code" , "item" = "item" , "year" = "year"))
ee1 <- ee %>% filter(year == 2017) %>%
  select(item , area , yield , area_harvested , production) %>%
  add_row(item = "Raw Bread (Pan crudo)", area = "Pancrudo" , yield = 104 , area_harvested = 1248 , production = 1.234)

datatable(data = ee1 , options = list(), class = "display",
    callback = JS("return table;"), rownames = FALSE , colnames = c('Cultivo (en inglés)', 'País/Región (en inglés)', 'Rendimiento (en hg/ha)' , 'Area cultivada (en hectáreas)' , 'Producción (en toneladas)') ,
    caption = NULL, filter = c("none", "bottom", "top"), escape = FALSE,
    style = "jqueryui", width = NULL, height = NULL, elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
    selection = c("multiple", "single", "none"), extensions = list(),
    plugins = "natural", editable = FALSE)
