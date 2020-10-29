setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
source("./Code/DataImport.R");
source("./Code/VisualizeFunctions.R");
data = import_Proef_Data()
general_variables = c("Accession", "Year", "HarvestTime", "Repetition","Bed", "Plot", "plant")

wrias_data = data_melt(data,"wrias","Sample",general_variables)
wruws_data = data_melt(data,"wruws","Sample",general_variables)
SG_data = merge(wrias_data, wruws_data)
SG_data$SG = SG_data$wrias / (SG_data$wrias - SG_data$wruws)

fw_data = data_melt(data,"fw","SizeSection",general_variables)
dw_data = data_melt(data,"dw","SizeSection",general_variables)
DS_data = merge(fw_data, dw_data)
DS_data$DS = DS_data$dw / DS_data$fw
DS_data$Size = as.factor(substr(DS_data$SizeSection,1,1))
DS_data$Section = as.factor(substr(DS_data$SizeSection,4,4))
DS_data$SizeSection = NULL

DS_data = DS_data[DS_data$fw > 9.75,]

for (var in c("DS","fw", "dw")){
  hist_plotly(DS_data, var, 200)
}

for (var in c("fw", "dw")){
  for (year in c(1, 2)){
    hist_plotly(DS_data[DS_data$Year == year,], var, 200)
  }
}

variables = c("HarvestTime", "Year", "Size", "Section", "Accession", "Repetition")
for (variable in variables){
  barplotly(DS_data, variable)
}

for (variable in c("Year", "Size")){
  marginal_plotly(DS_data, variable, "DS")
}

for (variable in variables){
  boxplotly(DS_data, variable, "DS")
}


for (variablex in variables){
  cross_variables = setdiff(variables, variablex)
    for (variabley in cross_variables){
      if (variablex == "HarvestTime" & variabley == "Repetition"){break}
      cross_boxplotly(DS_data, variablex, variabley, "DS", orientation = "x")
    }
}

