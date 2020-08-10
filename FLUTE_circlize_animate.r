#required packages
library(tidyverse)
library(circlize)
library(animation)

data_dir <- "/home/james/wdResearch/FLUTEoutput/Data/"
#scenario <- "scenario20_Const_Stest_2020-07-21a"

#set for the run in CRAFTY (althrough runID difficult to control)
scenario_list <- c(
   "scenario20_Const_Stest_2020-07-29b",
   "scenario20_YieldA_Stest_2020-07-29a",
   "scenario20_YieldB_Stest_2020-07-29a",
   "scenario20_rcp45_Stest_2020-07-29a",
   "scenario20_rcp85_Stest_2020-07-29a"
)

#scenario_lab <- "Const"

#set for the run in CRAFTY (althrough runID difficult to control)
scenario_lab_list <- c(
   "Const",
   "YieldA",
   "YieldB",
   "rcp45",
   "rcp85"
)

scenario <- "scenario20_rcp85_Stest_2020-08-05a"

#for(i in seq_along(scenario_list)){

#   scenario <- scenario_list[i]
#   scenario_lab <- scenario_lab_list[i]

sdat <- read_csv(paste0(data_dir, scenario, "/StellaData/CPImportToFrom.csv"))

cft_long <- sdat %>%
   rename(Variable=Years) %>%
   separate(Variable, into=c("first","CFT","last"),sep="\\[|\\]") %>%  #split on [ or ] again double escape
   dplyr::select(-first,-last) %>%
   separate(CFT, into=c("Commodity","To", "From"),sep="\\,") %>%  #split on , needs double escape (one for R, one for regex)
   pivot_longer(-c(Commodity,To,From), names_to="Year", values_to="Gg") %>%
   mutate(From = trimws(From)) %>%
   mutate(To = trimws(To)) %>%
   filter(!is.na(From)) #remove empty rows

regions <- c("CAN", "USA", "CCAmerica", "Brazil", "SOAmer", "REurope", "EU27", "OthCEECIS", "MEASNAfr", "SSAFR", 
             "Russia", "CHIHKG", "INDIA", "RSASIA", "EASIA", "Japan", "RSEASIA", "MALAINDO", "OCEANIA") 

rcols <- c("#C7821C", "#D7765B", "#CD7E37", "#00AABA", "#B177E2", "#A69100", "#AF8E00", "#65A100", "#DC726B", "#00A8C3", 
           "#909800", "#BD72DD", "#7D89E6", "#00A2D3", "#E16A86", "#00AC74", "#00ABB0", "#00AD8E", "#839B00")

names(rcols) <- regions

pdf_name <- paste0(data_dir,scenario,"/circlize.pdf")

pdf(file = pdf_name)

yrs <- c(2005,2015,2025)
cms <- c("Maize","OilCrop")

#loop commodities
for(cm in cms){
   
   #plotls <- ls() #reset plot list for animation
   
   
   #loop years
   for(j in seq_along(yrs)){
      
      cft <- cft_long %>%
         filter(Year == yrs[j]) %>%
         filter(Commodity == cm) %>%
         rename(Value=Gg) %>%
         dplyr::select(From, To, Value)  #get columns in the right order!
      
      #highlight links USA, BRA, CHI, SSAfr (gre remaining)
      #see https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html#highlight-links
      
      chordDiagram(cft,
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
}

dev.off()
