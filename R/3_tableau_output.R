# create dataframe with missing departments so they would show as NA in map
library(sf)
adm <- st_read("Input/shapefiles_unzipped/col_admbnda_adm1_mgn_20200416.shp") %>% 
  st_transform(crs = 4326)

departamentos_shp <- adm %>% select(ADM1_ES) %>%
  dplyr::rename(locality = ADM1_ES) 
departamentos_shp <- as.data.frame(departamentos_shp$locality)
names(departamentos_shp)[1] <- "locality"
departamentos_shp$comunidad_de_acogida <- NA
departamentos_shp$pendular <- NA
departamentos_shp$retornado <- NA
departamentos_shp$transito <- NA
departamentos_shp$vocacion_de_permanencia <- NA
departamentos_shp <- departamentos_shp %>% gather(gov_gaza, value, -locality)
departamentos_shp$value <- NULL



# reshape dataset and merge with missing departments
summary <- summary %>% select("dependent.var", "independent.var.value", 
                            "independent.var.value", "numbers", 
                            "repeat.var.value") %>%
  dplyr::rename(value_locality = numbers) %>%
  dplyr::rename(dependent.variable = dependent.var) %>%
  dplyr::rename(locality = repeat.var.value) %>%
  dplyr::rename(gov_gaza = independent.var.value) %>%
  merge(analysisplan, by="dependent.variable", 
        all = T) %>%
  select("dependent.variable", "gov_gaza", "value_locality", 
         "locality", "Indicator.Group...Sector", "research.question", 
         "sub.research.question", "repeat.for.variable") %>%
  mutate(locality = case_when(
            locality == "antioquia" ~ "Antioquia",
            locality == "arauca" ~ "Arauca",
            locality == "atlantico" ~ "Atlántico",
            locality == "bogota_dc" ~ "Bogotá, D.C.",
            locality == "bolivar" ~ "Bolívar", 
            locality == "cesar" ~ "Cesar", 
            locality == "cundinamarca" ~ "Cundinamarca", 
            locality == "la_guajira" ~ "La Guajira", 
            locality == "magdalena" ~ "Magdalena", 
            locality == "narino" ~ "Nariño", 
            locality == "norte_de_santander" ~ "Norte de Santander", 
            locality == "santander" ~ "Santander", 
            locality == "valle_del_cauca" ~ "Valle del Cauca"))

summary_short <- summary %>% select("locality", "gov_gaza", "dependent.variable", "value_locality") %>%
            reshape(idvar = c("locality", "gov_gaza"), timevar = "dependent.variable", direction = "wide")

            colnames(summary_short) <- gsub("value_locality.", "", colnames(summary_short))

#%>%
#write.csv("Output/tableau/department_tableau_output.csv")

  summary_short <- left_join(departamentos_shp, summary_short, by=c("locality", "gov_gaza"), all = T)
  summary_short <- summary_short %>% gather(dependent.var, numbers, -c(locality, gov_gaza))


# merge research questions with summary short
gaggi <-  summary %>% select("dependent.variable", "Indicator.Group...Sector", 
                     "research.question", "sub.research.question", 
                     "repeat.for.variable") %>% 
            dplyr::rename(dependent.var = dependent.variable) %>%
            left_join(summary_short, by = "dependent.var") %>%
            filter(locality != "Archipiélago de San Andrés, Providencia y Santa Catalina")


gaggi <- gaggi[!duplicated(gaggi), ]


write.xlsx(gaggi, "Output/tableau/department_tableau_output_full.xlsx")



# national level findings
adm0 <- st_read("Input/shapefiles_unzipped/col_admbnda_adm0_mgn_itos_20200416.shp") %>% 
  st_transform(crs = 4326)
summary$ADM0_ES <- "Colombia"
write.xlsx(summary, "Output/tableau/national_tableau_output_full.xlsx")

arsch <- response_with_composites %>%  
  group_by(departamento, pop_group) %>% dplyr::summarise(duration = mean(cari_insecurity, na.rm =T))
    