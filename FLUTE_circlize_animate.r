#required packages
library(tidyverse)
library(circlize)
library(animation)

data_dir <- "/home/james/wdResearch/FLUTEoutput/Data/"
#data_dir <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"

#scenario <- "flute_baseline_2020-08-06a"
scenario_list <- c("flute_baseline_2020-08-06b"
                   #"flute_demAPdecrA_2020-08-06b",
                   #"flute_yldCPshkA_2020-08-06b",
                   #"flute_demAPdecrA_yldCPshkA_2020-08-06b"
)

#scenario_lab <- "Baseline"
scenario_lab_list <- c(
   "Baseline",
   "Demand",
   "Yield",
   "Dem-Yld"
)


#regions to highlight links
highlights <- c("Brazil", "USA", "EU27", "CHIHKG", "SSAFR")



for(i in seq_along(scenario_list)){

   scenario <- scenario_list[i]
#   scenario_lab <- scenario_lab_list[i]

   cpdat <- read_csv(paste0(data_dir, scenario, "/StellaData/CPImportToFrom.csv"))
   
   cpft_long <- cpdat %>%
      rename(Variable=Years) %>%
      separate(Variable, into=c("first","CFT","last"),sep="\\[|\\]") %>%  #split on [ or ] again double escape
      dplyr::select(-first,-last) %>%
      separate(CFT, into=c("Commodity","To", "From"),sep="\\,") %>%  #split on , needs double escape (one for R, one for regex)
      pivot_longer(-c(Commodity,To,From), names_to="Year", values_to="Gg") %>%
      mutate(From = trimws(From)) %>%
      mutate(To = trimws(To)) %>%
      filter(!is.na(From)) #remove empty rows
   
   apdat <- read_csv(paste0(data_dir, scenario, "/StellaData/APImportToFrom.csv"))
   
   apft_long <- apdat %>%
      rename(Variable=Years) %>%
      separate(Variable, into=c("first","CFT","last"),sep="\\[|\\]") %>%  #split on [ or ] again double escape
      dplyr::select(-first,-last) %>%
      separate(CFT, into=c("Commodity","To", "From"),sep="\\,") %>%  #split on , needs double escape (one for R, one for regex)
      pivot_longer(-c(Commodity,To,From), names_to="Year", values_to="Gg") %>%
      mutate(From = trimws(From)) %>%
      mutate(To = trimws(To)) %>%
      filter(!is.na(From)) %>%  #remove empty rows
      mutate(Commodity = if_else(Commodity == "Pig", "Swine", "Meat")) 
   
   
   regions <- c("CAN", "USA", "CCAmerica", "Brazil", "SOAmer", "REurope", "EU27", "OthCEECIS", "MEASNAfr", "SSAFR", 
                "Russia", "CHIHKG", "INDIA", "RSASIA", "EASIA", "Japan", "RSEASIA", "MALAINDO", "OCEANIA") 
   
   rcols <- c("#C7821C", "#e41a1c", "#CD7E37", "#4daf4a", "#B177E2", "#A69100", "#377eb8", "#65A100", "#DC726B", "#984ea3", 
              "#909800", "#ff7f00", "#7D89E6", "#00A2D3", "#E16A86", "#00AC74", "#00ABB0", "#00AD8E", "#839B00")
   
   #uniform grey
   greycol <- rep("lightgrey", 19)
   
   #for regions not in highlights list, set grey
   rcols[!regions %in% highlights] <- greycol[!regions %in% highlights]
   
   names(rcols) <- regions
   
   
   yrs <- seq(2001,2030,1)
   
   #crops
   cms <- c("Maize","OilCrop")
   
   #loop commodities
   for(cm in cms){
      
      saveGIF({
      
         #loop years
         for(j in seq_along(yrs)){
            
            cpft <- cpft_long %>%
               filter(Year == yrs[j]) %>%
               filter(Commodity == cm) %>%
               rename(Value=Gg) %>%
               dplyr::select(From, To, Value)  #get columns in the right order!
           
           
            chordDiagram(cpft,
                         annotationTrack = "grid",
                         grid.col = rcols,
                         transparency = 0.25,
                         preAllocateTracks = list(track.height = 0.2), 
                         directional = 1, 
                         direction.type = c("diffHeight", "arrows"), 
                         link.arr.type = "big.arrow")
            
            title(main= paste0(yrs[j], ", ",cm))
            circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim")
               ylim = get.cell.meta.data("ylim")
               sector.name = get.cell.meta.data("sector.index")
               circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                           niceFacing = TRUE, adj = c(-0.25, 0.5))
               circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
            }, bg.border = NA) # here set bg.border to NA is important
         }
      }, movie.name=paste0(data_dir,scenario,"/",scenario,"-",cm,"_",head(yrs,1),"-",tail(yrs,1),".gif"),autobrowse=F, ani.res=125, ani.width=960, ani.height=960)
   }
   
   
   
   #animal products
   cms <- c("Meat","Swine")

   #loop commodities
   for(cm in cms){
      saveGIF({
         
         #loop years
         for(j in seq_along(yrs)){
            
            apft <- apft_long %>%
               filter(Year == yrs[j]) %>%
               filter(Commodity == cm) %>%
               rename(Value=Gg) %>%
               dplyr::select(From, To, Value)  #get columns in the right order!
            
            
            chordDiagram(apft,
                         annotationTrack = "grid",
                         grid.col = rcols,
                         transparency = 0.25,
                         preAllocateTracks = list(track.height = 0.2), 
                         directional = 1, 
                         direction.type = c("diffHeight", "arrows"), 
                         link.arr.type = "big.arrow")
            
            title(main= paste0(yrs[j], ", ",cm))
            circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim")
               ylim = get.cell.meta.data("ylim")
               sector.name = get.cell.meta.data("sector.index")
               circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                           niceFacing = TRUE, adj = c(-0.25, 0.5))
               circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
            }, bg.border = NA) # here set bg.border to NA is important
         }
      }, movie.name=paste0(data_dir,scenario,"/",scenario,"-",cm,"_",head(yrs,1),"-",tail(yrs,1),".gif"),autobrowse=F, ani.res=125, ani.width=960, ani.height=960)

   }
}

