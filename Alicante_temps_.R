
library(climaemet)

# 7119B - CARAVACA DE LA CRUZ
# 7178I      "08430"  MURCIA
# 7228       "08429"  ALCANTARILLA, BASE AÉREA
# 5402       "08410"  CÓRDOBA AEROPUERTO
# 5390Y      "08346"  VILLANUEVA DE CÓRDOBA
# 3194U      "08220"  MADRID, CIUDAD UNIVERSITARIA
climatestripes_station(
  "3194U",
  start = 1951,
  end = 2022,
  with_labels = "yes",
  col_pal = "Inferno"
)


alidata <- aemet_monthly_period(
  station = "3194U",
  start = 1951,
  end = 2022,
  verbose = TRUE,
  return_sf = FALSE
)

library(data.table)
library(tidytable)
library(stringi)
library(ggplot2)

alidatadt <- alidata %>%
  as.data.table()


alijuly <- alidatadt %>%
  filter.( fecha %like% "-8") %>%
  filter.( !is.na(ta_max) ) %>%
  select.( fecha, ta_max) %>%
  mutate.( mimax = stri_replace_all_fixed(ta_max, "(", " (")) %>%
  mutate.( mimaxdef = as.double(stri_extract_first_words(mimax))) %>%
  select.( fecha, mimaxdef) %>%
  mutate.( year = as.integer(stri_replace_all_fixed(fecha, "-8", ""))) %>%
  rename.( temp = mimaxdef) %>%
  select.(-fecha) %>%
  relocate.(year, .before = temp) %>%
  as.data.table()


ggstripes(
  alijuly, 
  plot_title = "TEMPERATURAS MÁXIMAS AGOSTO - ALICANTE", 
  plot_type = "trend") +
  labs(subtitle = "(1951-2021)",
       theme(element_line(color = "yellow"))
  )
ggsave("Alicante_Temp_Maximas_Agosto_.png")

