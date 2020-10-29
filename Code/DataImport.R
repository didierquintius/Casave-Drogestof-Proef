library(readxl)
library(dplyr)
library(reshape2)
library(plotly)
setwd("D:/Stage CELOS/Casave Drogestof Proef")

col_start_with = function(data, prefix){
  colnames(data)[startsWith(colnames(data),prefix)]
}

# initialize dataframe
data = data.frame()

import_Proef_Data = function(){

    # loop over all sheets of the Excel Data file
  for (i in 1:3){
    for (j in 1:3){
      # import the data one sheet at a time
      imported_data = read_excel("D:/Stage CELOS/Casave Drogestof Proef/Data/CasaveDrogeStofProefInput.xlsx", sheet = paste("O",i," H",j, sep = ""))
      
      # add a column with Harvest Time of the imported data
      imported_data$HarvestTime = 9 + i  + (j - 1) * 0.25
      
      # add the repetition as this codes for the location of the 
      imported_data$Repetition = j
      
      # add the data from one sheet to the rest of the data frame
      data = rbind(data, imported_data)
    }
  }
  
  data
}

data_melt = function(data, melt_variable_prefix, suffix_variable, general_variables){
  
  # put the data from all the prefix measurements in one column
  data = melt(data,id.vars = general_variables, measure.vars = col_start_with(data,melt_variable_prefix),
                    value.name = melt_variable_prefix, variable.name = suffix_variable)

  # remove all variables with NA value for wrias
  data = data[!is.na(data[,melt_variable_prefix]),]
  
  for (col in general_variables){
    data[, col] = as.factor(data[, col])
  }
  data[, suffix_variable] = substr(as.character(data[,suffix_variable]),nchar(melt_variable_prefix) + 1,20)
  
  data
}

