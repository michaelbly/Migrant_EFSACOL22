#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$stratum <- paste(samplingframe$departamento, samplingframe$popgroup, sep = "_")


#ADD STRATA NAMES TO DATA 

## create strata for response dataset
response <- response %>% 
  mutate(strata = paste(departamento,pop_group, sep ="_")) %>% 
  filter(!strata %in% c("narino_pendular", "la_guajira_transito"))




##CHECK IF ALL MATCH SAMPLINGFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(response$strata %in% samplingframe$stratum))){
  warning("some strata not found in samplingframe")
  warning(which(!(response$strata %in% samplingframe$stratum)) %>% length)
}
response$strata %find those not in% samplingframe$stratum


if(any(is.na(response$strata))){
  warning("strata can not be NA")
}

