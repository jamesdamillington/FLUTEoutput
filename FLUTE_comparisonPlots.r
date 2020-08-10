#required packages
library(tidyverse)

#data_dir <- "/home/james/wdResearch/FLUTEoutput/Data/"
data_dir <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"
#scenario <- "flute_baseline_2020-08-06a"

scenario_list <- c("flute_baseline_2020-08-06a",
                   "flute_demAPdecrA_2020-08-06a",
                   "flute_yldCPshkA_2020-08-06a",
                   "flute_demAPdecrA_yldCPshkA_2020-08-06a"
)

#scenario_lab <- "baseline"

#set for the run in CRAFTY (althrough runID difficult to control)
scenario_lab_list <- c(
   "Baseline",
   "Demand",
   "Yield",
   "Dem-Yld"
)


#unique(sdat_prodn$Region)
regions <- c("USA", "EU 27", "CHIHKG", "S S AFR")


pdfprint <- T

if(length(scenario_list) > 1) {
   pdf_name <- paste0(data_dir,paste0(scenario_lab_list,collapse="-"),"_comparisonPlots_FLUTE.pdf")
} else
{
   pdf_name <- paste0(data_dir,scenario_list[1],"/",scenario_lab_list[1],"_comparisonPlots_FLUTE.pdf")
}
   

read_cDat <- function(LC_name) {
   
   cDat <- readr::read_csv(LC_name, col_types = cols(
      muniID = col_integer(),	
      FR1	= col_double(),
      FR2	= col_double(),	
      FR3	= col_double(),	
      FR45	= col_double(),
      FR6	= col_double(),	
      FR7	= col_double(),	
      FR8	= col_double(),	
      cellCount = col_integer(),	
      Year = col_integer(),	
      ModMode = col_integer()))
   
   return(cDat)
}


######
#STELLA DATA

#empty table to populate from files below
empty_sdat <- data.frame(
   Region = character(),
   Commodity = character(),
   Year = integer(),
   Gg = numeric(),
   Measure = character(),
   Scenario = character()
)
as_tibble(empty_sdat)

sdat_prodn_all <- empty_sdat
sdat_export_all <- empty_sdat

sdat_area_all <- empty_sdat
sdat_area_all <- sdat_area_all %>%
   rename(ha=Gg) %>%
   select(-Measure)

#i <- 4
for(i in seq_along(scenario_list)){

   scenario <- scenario_list[i]
   scenario_lab <- scenario_lab_list[i]
   print(scenario_lab) 


   sdat <- read_csv(paste0(data_dir, scenario, "/StellaData/Outputs.csv"))
   
   sdat_prodn <- sdat %>%
      rename(Variable=Years) %>%
      dplyr::filter(grepl("prodn", Variable)) %>%
      separate(Variable, into=c("Region","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
      separate(Variable, into=c("first","Commodity","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
      dplyr::select(-first,-last) %>%
      pivot_longer(-c(Region,Commodity), names_to="Year", values_to="Gg") %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate(Measure = "Production") %>%
      mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy")) %>%
      mutate(Scenario=scenario_lab) %>%
      filter(Region %in% regions)
   
   sdat_export <- sdat %>%
      rename(Variable=Years) %>%
      filter(grepl("export", Variable)) %>%
      separate(Variable, into=c("Region","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
      separate(Variable, into=c("first","Commodity","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
      dplyr::select(-first,-last) %>%
      pivot_longer(-c(Region,Commodity), names_to="Year", values_to="Gg") %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate(Measure = "Export") %>%
      mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy")) %>%
      mutate(Scenario=scenario_lab) %>%
      filter(Region %in% regions)
   
   sdat_area <- sdat %>%
      rename(Variable=Years) %>%
      filter(grepl("crop lands", Variable)) %>%
      separate(Variable, into=c("Region","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
      separate(Variable, into=c("first","Commodity","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
      dplyr::select(-first,-last) %>%
      pivot_longer(-c(Region,Commodity), names_to="Year", values_to="ha") %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy")) %>%
      mutate(Scenario=scenario_lab) %>%
      filter(Region %in% regions)
   
   sdat_pas <- sdat %>%
      rename(Variable=Years) %>%
      filter(grepl("Forage Land", Variable)) %>%
      separate(Variable, into=c("Region","Variable"),sep="\\.") %>%
      mutate(Commodity = "Pasture") %>%
      select(-Variable) %>%
      pivot_longer(-c(Region,Commodity), names_to="Year", values_to="ha") %>%
      mutate(Year = as.numeric(Year)) %>%
      mutate(Scenario=scenario_lab) %>%
      filter(Region %in% regions)
   
   sdat_area <- bind_rows(sdat_area, sdat_pas)
   
   
   if(i == 1) {
      sdat_prodn_all <- sdat_prodn
      sdat_export_all <- sdat_export
      sdat_area_all <- sdat_area
   } else {
      sdat_prodn_all <- bind_rows(sdat_prodn_all, sdat_prodn)
      sdat_export_all <- bind_rows(sdat_export_all, sdat_export)
      sdat_area_all <- bind_rows(sdat_area_all, sdat_area)
   }
   
   #following does not work as Actual Forage Land has different format to commodities (what is Actual Forage Land??)
   #move land area to a different script
   #sdat_land <- sdat %>%
   #   rename(Variable=Years) %>%
   #   filter(grepl("and", Variable)) %>%
   #   separate(Variable, into=c("Region","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
   #   separate(Variable, into=c("first","Variable","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
   #   dplyr::select(-first,-last) %>%
   #   pivot_longer(-c(Region,Variable), names_to="Year", values_to="ha")
}


   
######
#CRAFTY DATA
yrs <- unique(sdat_prodn_all$Year)

#empty table to populate from files below
empty_cdat <- data.frame(
   Region = character(),
   Commodity = character(),
   Year = integer(),
   Gg = numeric(),
   Measure = character(),
   Scenario = character()
)
as_tibble(empty_cdat)

cdat_prodn_all <- empty_cdat
cdat_export_all <- empty_cdat

cdat_area_all <- empty_cdat
cdat_area_all <- cdat_area_all %>%
   rename(ha = Gg) %>%
   select(-Measure)

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
         add_row(Region = "BRA", Commodity = "Soy", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[1,2])) %>%
         add_row(Region = "BRA", Commodity = "Soy", Measure = "Storage", Year = yrs[i], Gg = as.numeric(dat[4,2])) %>%
         add_row(Region = "BRA", Commodity = "Soy", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[3,2])) %>%
         add_row(Region = "BRA", Commodity = "Maize", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[6,2])) %>%
         add_row(Region = "BRA", Commodity = "Maize", Measure = "Storage", Year = yrs[i], Gg = as.numeric(dat[9,2])) %>%
         add_row(Region = "BRA", Commodity = "Maize", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[8,2])) %>%
         add_row(Region = "BRA", Commodity = "Meat", Measure = "Production", Year = yrs[i], Gg = as.numeric(dat[11,2])) %>%
         add_row(Region = "BRA", Commodity = "Meat", Measure = "Export", Year = yrs[i], Gg = as.numeric(dat[14,2])) 
      #%>%
      #add_row(Commodity = "Dairy", Measure = "Production", Year = yrs[i], value_gg = as.numeric(dat[12,2])) %>%
      #add_row(Commodity = "Dairy", Measure = "Export", Year = yrs[i], value_gg = as.numeric(dat[16,2]))
      
   }
   
   cdat <- mutate(cdat, Scenario=scenario_lab)
   
   cdat_prodn <- cdat %>%
      filter(Measure == "Production") %>%
      #dplyr::select(-Measure) %>%
      mutate(Scenario=scenario_lab)

   cdat_export <- cdat %>%
      filter(Measure == "Export") %>%
      #dplyr::select(-Measure) %>%
      mutate(Scenario=scenario_lab)
   
   
   raw_area <- read_cDat(paste0(data_dir,scenario,"/0-0/",scenario,"_States-All_CRAFTYmunisLC.csv"))
   
   cdat_area <- raw_area %>%
      mutate(Soy.Mod = round(FR1 * cellCount * 2500,0)) %>%
      mutate(Mze.Mod = round(FR2 * cellCount * 2500,0)) %>%
      mutate(DC.Mod = round(FR3 * cellCount * 2500,0)) %>%
      mutate(Nat.Mod = round(FR45 * cellCount * 2500,0)) %>%
      mutate(OAgri.Mod = round(FR6 * cellCount * 2500,0)) %>%
      mutate(Other.Mod = round(FR7 * cellCount * 2500,0)) %>%
      mutate(Pas.Mod = round(FR8 * cellCount * 2500,0))
   
   cdat_area <- cdat_area %>%
      dplyr::select(Year, Soy.Mod:Pas.Mod) %>%
      pivot_longer(-c(Year), names_to="Commodity", values_to="ha") %>%
      #gather(key = LUC, value = sqkm, -Year) %>% 
      group_by(Year,Commodity) %>%
      summarise_at(vars(matches("ha")),sum, na.rm=TRUE) %>%
      filter(Commodity == "Soy.Mod" | Commodity == "Pas.Mod" | Commodity == "Mze.Mod" | Commodity == "DC.Mod") %>%
      mutate(Commodity = if_else(Commodity == "Soy.Mod", "Soy", 
                                 if_else(Commodity == "Pas.Mod", "Pasture",
                                         if_else(Commodity == "Mze.Mod", "Maize", "DC")))) %>%
      mutate(Region = "BRA") %>%
      mutate(Scenario = scenario_lab) %>%
      select(Region, Commodity, Year, ha, Scenario)
      
   if(i == 1) {
      cdat_prodn_all <- cdat_prodn
      cdat_export_all <- cdat_export
      cdat_area_all <- cdat_area
   } else {
      cdat_prodn_all <- bind_rows(cdat_prodn_all, cdat_prodn)
      cdat_export_all <- bind_rows(cdat_export_all, cdat_export)
      cdat_area_all <- bind_rows(cdat_area_all, cdat_area)
   }
}

all_prodn <- bind_rows(sdat_prodn_all, cdat_prodn_all)

all_export <- bind_rows(sdat_export_all, cdat_export_all)

all_area <- bind_rows(sdat_area_all, cdat_area_all)

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
      facet_grid(Scenario~Region) +
      ggtitle("Multi Scenarios")
   print(multiprod)
   
   multiexport <- ggplot(all_export, aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Export (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(Scenario~Region) +
      ggtitle("Multi Scenarios")
   print(multiexport)
   
   multiarea <- all_area %>%
      mutate(mha = ha / 1000000) %>%
      ggplot(aes(x=Year,y=mha,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Area (million ha)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(Scenario~Region) +
      ggtitle("Multi Scenarios")
   print(multiarea)
}

#single scenarios
for(i in seq_along(scenario_list)){
   
   prodplot <- all_prodn %>%
      filter(Scenario==scenario_lab_list[i]) %>%
      ggplot(aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Production (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(.~Region) +
      ggtitle(paste0("Scenario: ",scenario_lab_list[i]))
   print(prodplot)

   expplot <- all_export %>%
      filter(Scenario==scenario_lab_list[i]) %>%
      ggplot(aes(x=Year,y=Gg,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Export (Gg)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(.~Region) +
      ggtitle(paste0("Scenario: ",scenario_lab_list[i]))
   print(expplot)
   
   areaplot <- all_area %>%
      filter(Scenario==scenario_lab_list[i]) %>%
      mutate(mha = ha / 1000000) %>%
      ggplot(aes(x=Year,y=mha,group=Commodity)) +
      geom_line(aes(colour=Commodity)) +
      scale_y_continuous(name = "Area (million ha)", labels = scales::comma) +
      scale_x_continuous(breaks=c(2005,2015,2025)) +
      facet_grid(.~Region) +
      ggtitle(paste0("Scenario: ",scenario_lab_list[i]))
   print(areaplot)

}

if(pdfprint) {
   dev.off()
}
