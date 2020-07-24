
#required packages
library(tidyverse)

data_dir <- "/home/james/wdResearch/FLUTEoutput/Data/"
scenario <- "scenario20_Const_Stest_2020-07-21a"

pdfprint <- T
pdf_name <- paste0(data_dir,scenario,"/",scenario,"_ProductionExport_FLUTE.pdf")

######
#STELLA DATA
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
   mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy"))

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
   mutate(Commodity=replace(Commodity, Commodity == "OilCrop", "Soy"))


#following does not work as Actual Forage Land has different format to commodities (what is Actual Forage Land??)
#move land area to a different script
#sdat_land <- sdat %>%
#   rename(Variable=Years) %>%
#   filter(grepl("and", Variable)) %>%
#   separate(Variable, into=c("Country","Variable"),sep="\\.") %>%   #split on . needs double escape (one for R, one for regex)
#   separate(Variable, into=c("first","Variable","last"),sep="\\[|\\]") %>%  #split on [ or ] again needs double escape
#   dplyr::select(-first,-last) %>%
#   pivot_longer(-c(Country,Variable), names_to="Year", values_to="ha")


######
#CRAFTY DATA
yrs <- unique(sdat_prodn$Year)

#empty table to populate from files below
cdat <- data.frame(
   Country = character(),
   Commodity = character(),
   Year = integer(),
   Gg = numeric(),
   Measure = character()
)
as_tibble(cdat)

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

cdat_prodn <- cdat %>%
   filter(Measure == "Production") %>%
   dplyr::select(-Measure)

all_prodn <- bind_rows(sdat_prodn, cdat_prodn)
        

cdat_export <- cdat %>%
   filter(Measure == "Export") %>%
   dplyr::select(-Measure)

all_export <- bind_rows(sdat_export, cdat_export)



######
#PLOTTING

if(pdfprint) {
   pdf(file = pdf_name, width=8, height=6)
}

prodplot <- ggplot(all_prodn, aes(x=Year,y=Gg,group=Commodity)) +
   geom_line(aes(colour=Commodity)) +
   scale_y_continuous(name = "Production (Gg)", labels = scales::comma) +
   scale_x_continuous(breaks=c(2005,2015,2025)) +
   facet_grid(.~Country) +
   ggtitle("Production")
print(prodplot)

expplot <- ggplot(all_export, aes(x=Year,y=Gg,group=Commodity)) +
   geom_line(aes(colour=Commodity)) +
   scale_y_continuous(name = "Export (Gg)", labels = scales::comma) +
   scale_x_continuous(breaks=c(2005,2015,2025)) +
   facet_grid(.~Country) +
   ggtitle("Export")
print(expplot)

if(pdfprint) {
   dev.off()
}
