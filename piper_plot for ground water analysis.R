install.packages("remotes")

remotes::install_github("USGS-R/smwrData")
remotes::install_github("USGS-R/smwrBase")
remotes::install_github("USGS-R/smwrGraphs")

library(smwrGraphs)
library(smwrData)

data(MiscGW)

# Convert concentrations to milliequivalents per liter
PD <- transform(MiscGW, 
                Ca.meq = conc2meq(Calcium, "calcium"),
                Mg.meq = conc2meq(Magnesium, "magnesium"),
                Na.meq = conc2meq(Sodium, "sodium"),
                Cl.meq = conc2meq(Chloride, "chloride"),
                SO4.meq = conc2meq(Sulfate, "sulfate"),
                HCO3.meq = conc2meq(Bicarbonate, "bicarb"))


PD$SS <- row.names(PD) 

piperPlot(PD$Ca.meq, PD$Mg.meq, PD$Na.meq, 
          PD$Cl.meq, PD$HCO3.meq, PD$SO4.meq, 
          Plot=list(name=PD$SS, color=setColor(PD$SS)), 
          zCat.title = "Sodium", 
          xAn.title = "Chloride", 
          yAn.title = "Bicarbonate")



