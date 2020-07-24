rm(list=ls())

#required packages
library(tidyverse)

data_dir <- "/home/james/wdResearch/FLUTEoutput/Data/"
#scenario <- "scenario20_Const_Stest_2020-07-21a"

#set for the run in CRAFTY (althrough runID difficult to control)
scenario_list <- c(
   "scenario20_rcp45_Stest_2020-07-24a",
   "scenario20_Const_Stest_2020-07-21a"
)

#scenario_lab <- "Const"

#set for the run in CRAFTY (althrough runID difficult to control)
scenario_lab_list <- c(
   "rcp45",
   "Const"
)

pdfprint <- T

if(length(scenario_list) > 1) {
   pdf_name <- paste0(data_dir,paste0(scenario_lab_list,collapse="-"),"_ProductionExport_FLUTE.pdf")
} else
{
   pdf_name <- paste0(data_dir,scenario_list[1],"/",scenario_lab_list[1],"_ProductionExport_FLUTE.pdf")
}
   


######
#STELLA DATA

#empty table to populate from files below
empty_sdat <- data.frame(
   Country = character(),
   Commodity = character(),
   Year = integer(),
   Gg = numeric(),
   Scenario = character()
)
as_tibble(empty_sdat)

sdat_prodn_all <- empty_sdat
sdat_export_all <- empty_sdat

for(i in seq_along(scenario_list)){

   scenario <- scenario_list[i]
   scenario_lab <- scenario_lab_list[i]
   print(scenario_lab) 


   sdat <- read_csv(paste0(data_dir, scenario, "/StellaData/Outputs.csv"))
   
   sdat_prodn <- sdat %>%
      rename(Variable=Years) %>%
      dplyr::filter(grepl("prodn", Variable)) %>%
      separate(Variable, into=c("Country","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
      separate(Variable, into=c("first","Commodity","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
      dplyr::select(-first,-last) %>%
      pivot_longer(-c(Country,Commodity), names_to="Year", values_to="Gg") %>%
      #mutate(Gg = mtons / 1000) %>% 
      mutate(Year = as.numeric(Year)) %>%
      #dplyr::select(-mtons) %>%
      mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy")) %>%
      mutate(Scenario=scenario_lab)
   
   sdat_export <- sdat %>%
      rename(Variable=Years) %>%
      filter(grepl("export", Variable)) %>%
      separate(Variable, into=c("Country","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
      separate(Variable, into=c("first","Commodity","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
      dplyr::select(-first,-last) %>%
      pivot_longer(-c(Country,Commodity), names_to="Year", values_to="Gg") %>%
      #mutate(Gg = mtons / 1000) %>% 
      mutate(Year = as.numeric(Year)) %>%
      #dplyr::select(-mtons) %>%
      mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy")) %>%
      mutate(Scenario=scenario_lab) 
   
   
   if(i == 1) {
      sdat_prodn_all <- sdat_prodn
      sdat_export_all <- sdat_export
   } else {
      sdat_prodn_all <- bind_rows(sdat_prodn_all, sdat_prodn)
      sdat_export_all <- bind_rows(sdat_export_all, sdat_export)
   }
   
   #following does not work as Actual Forage Land has different format to commodities (what is Actual Forage Land??)
   #move land area to a different script
   #sdat_land <- sdat %>%
   #   rename(Variable=Years) %>%
   #   filter(grepl("and", Variable)) %>%
   #   separate(Variable, into=c("Country","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
   #   separate(Variable, into=c("first","Variable","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
   #   dplyr::select(-first,-last) %>%
   #   pivot_longer(-c(Country,Variable), names_to="Year", values_to="ha")
}


   
######
#CRAFTY DATA
yrs <- unique(sdat_prodn_all$Year)

#empty table to populate from files below
empty_cdat <- data.frame(
   Country = character(),
   Commodity = character(),
   Year = integer(),
   Gg = numeric(),
   Measure = character(),
   Scenario = character()
)
as_tibble(empty_cdat)

cdat_prodn_all <- empty_sdat
cdat_export_all <- empty_sdat

for(i in seq_along(scenario_list)){
   
   scenario <- scenario_list[i]
   scenario_lab <- scenario_lab_list[i]
   print(scenario_lab) 

   cdat <- empty_cdat
   
   #loop through all files 
   for(i in seq_along(yrs)){
      
      filen <- paste0("0_FromMaestro",yrs[i],"_",scenario,".csv")
      
      dat <- read_csv(paste0(data_dir,scenario,"/StellaData/",filen),col_names=F)
      
      cdat <- cdat %>% 
         add_row(Country = "BRA", Commodity = "Soy", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[1,2])) %>%
         add_row(Country = "BRA", Commodity = "Soy", Measure = "Storage", Year = yrs[i], Gg = as.numeric(dat[4,2])) %>%
         add_row(Country = "BRA", Commodity = "Soy", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[3,2])) %>%
         add_row(Country = "BRA", Commodity = "Maize", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[6,2])) %>%
         add_row(Country = "BRA", Commodity = "Maize", Measure = "Storage", Year = yrs[i], Gg = as.numeric(dat[9,2])) %>%
         add_row(Country = "BRA", Commodity = "Maize", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[8,2])) %>%
         add_row(Country = "BRA", Commodity = "Meat", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[11,2])) %>%
         add_row(Country = "BRA", Commodity = "Meat", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[14,2])) 
      #%>%
      #add_row(Commodity = "Dairy", Measure = "Production", Year = yrs[i], value_gg = as.numeric(dat[12,2])) %>%
      #add_row(Commodity = "Dairy", Measure = "Export", Year = yrs[i], value_gg = as.numeric(dat[16,2]))
      
   }
   
   cdat <- mutate(cdat, Scenario=scenario_lab)
   
   cdat_prodn <- cdat %>%
      filter(Measure == "Production") %>%
      dplyr::select(-Measure) %>%
      mutate(Scenario=scenario_lab)

   cdat_export <- cdat %>%
      filter(Measure == "Export") %>%
      dplyr::select(-Measure) %>%
      mutate(Scenario=scenario_lab)
   
   if(i == 1) {
      cdat_prodn_all <- cdat_prodn
      cdat_export_all <- cdat_export
   } else {
      cdat_prodn_all <- bind_rows(cdat_prodn_all, cdat_prodn)
      cdat_export_all <- bind_rows(cdat_export_all, cdat_export)
   }
}

all_prodn <- bind_rows(sdat_prodn_all, cdat_prodn_all)

all_export <- bind_rows(sdat_export_all, cdat_export_all)

######
#PLOTTING

if(pdfprint) {
   pdf(file = pdf_name, width=8, height=6)
}

#multi-scenario
if(length(scenario_list) > 1) {
   multiprod <- ggplot(all_prodn, aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Production (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(Scenario~Country) +
      ggtitle("Multi Scenarios")
   print(multiprod)
   
   multiexport <- ggplot(all_export, aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Exportn (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(Scenario~Country) +
      ggtitle("Multi Scenarios")
   print(multiexport)
}

#single scenarios
for(i in seq_along(scenario_list)){
   
   prodplot <- all_prodn %>%
      filter(Scenario==scenario_lab_list[i]) %>%
      ggplot(aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Production (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(.~Country) +
      ggtitle(paste0("Scenario: ",scenario_lab_list[i]))
   print(prodplot)

   expplot <- all_export %>%
      filter(Scenario==scenario_lab_list[i]) %>%
      ggplot(aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Export (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(.~Country) +
      ggtitle(paste0("Scenario: ",scenario_lab_list[i]))
   print(expplot)

}

if(pdfprint) {
   dev.off()
}
