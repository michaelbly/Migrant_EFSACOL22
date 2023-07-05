loop_with_composites <- response_with_composites %>% 
  select(registro, weights, pop_group, sa8_sa, sa8_sam, sa8_iam, sa8_ias) %>%
  right_join(loop, by="registro") %>%
  mutate(sex_age_group = case_when(
    sexo == "mujer" & edad >= 18 & edad_anos_meses == "anos" ~ "adulto_mujer",
    sexo == "hombre" & edad >= 18 & edad_anos_meses == "anos" ~ "adulto_hombre",
    sexo == "mujer" & edad <  18 & edad_anos_meses == "anos" ~ "nino_mujer",
    sexo == "hombre" & edad <  18 & edad_anos_meses == "anos" ~ "nino_hombre",
    sexo == "hombre" & edad_anos_meses == "meses" ~ "nino_hombre",
    sexo == "mujer" & edad_anos_meses == "meses" ~ "nino_mujer"
  )) %>%
  drop_na(weights)



r <- loop %>% 
  mutate(ninos_10 = ifelse(edad < 10 & sexo "mujer", 1,0),
         ninas_10 = ifelse(edad < 10 & sexo "hombre", 1,0)) %>%
  group_by(registro) %>%
  dplyr::summarise(nr_ninos_10 = sum(ninos_10),
                   nr_ninas_10 = sum(ninas_10)) %>%
  select(starts_with("nr_nin"), registro) %>%
  right_join(r)




loop_with_composites <- response_with_composites %>% 
  select(registro, weights, pop_group, cari_categories) %>%
  right_join(loop, by="registro") %>%
  mutate(
    adulto_mujer = ifelse(sexo == "mujer" & edad >= 18 & edad_anos_meses == "anos",1,0),
    adulto_hombre = ifelse(sexo == "hombre" & edad >= 18 & edad_anos_meses == "anos",1,0),
    nino_mujer = ifelse((sexo == "mujer" & edad < 18 & edad_anos_meses == "anos") | 
                          (sexo == "mujer" & edad_anos_meses == "meses"),1,0),
    nino_hombre = ifelse((sexo == "hombre" & edad < 18 & edad_anos_meses == "anos") | 
                          (sexo == "hombre" & edad_anos_meses == "meses"),1,0)
  ) %>% 
  mutate(cari_categories_agg = case_when(
    cari_categories %in% c("iam", "ias") ~ "inseguridad",
    cari_categories %in% c("sa", "sam") ~ "seguridad",
    is.na(cari_categories) ~ "not_available"
  )) %>%
  drop_na(cari_categories_agg) %>%
  drop_na(weights)


weight_fun_loop<-function(df){
  loop_with_composites$weights
}


#LOAD ANALYSISPLAN
dap_name <- "loop_analysis"
analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
#analysisplan$independent.variable <-  "tiempo_en_pais"
analysisplan$repeat.for.variable <- "one"
loop_with_composites$one <- "one"
analysisplan$independent.variable <- "one"




result <- from_analysisplan_map_to_output(loop_with_composites, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "percentage_ind_by CARI_popgroupagg_nacional"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
summary$max <- ifelse(summary$numbers < 1 & summary$max > 1, 1, 
                      summary$max)
summary$min <- ifelse(summary$min < 0, 0, summary$min)

#summary$max <- ifelse(summary$max > 1, 1, summary$max)
#summary$max <- NULL
#summary$min <- ifelse(summary$min < 0, 0, summary$min)
#summary$min <- NULL
#summary$numbers <- as.character(as.numeric(round(summary$numbers,1)))


write.csv(summary, sprintf("Output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)


for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("Output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE, showNA = F)
  } else {
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE, showNA = F)
  }
}

