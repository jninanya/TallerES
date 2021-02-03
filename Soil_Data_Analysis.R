#===================================#
# Descriptive Analysis of Soil Data #
#===================================#

## Obs: In the Excel file (shet soil) add a new column
##      (called it "Select") and select (whit x) the 
##      variable of your interest.

#-------------------------#
# Clean the R environment #
#-------------------------#

rm(list = ls())

file = "La Victoria - Test"  ## Change it by your file's name

#-------------------------------------------------#
# Loading libraries, work diretory and Excel file #
#-------------------------------------------------#

# Libraries #
library(openxlsx)

# Work directory #
DW = getwd()
nDW = nchar(DW) - 7

wd.path = substr(DW, 1, nDW)

# Excel file #
dfr = read.xlsx(xlsxFile = paste0(wd.path, "Data/", file, ".xlsx") , sheet = "Soil")

#---------------------#
# Selecting variables #
#---------------------#

names(dfr) = tolower(names(dfr))
name = c("abbreviture", "unit", "data1", "data2", "data3", "select")

# Selecting #
new.dfr = dfr[!is.na(dfr$select), name]

#-------------------------------------------------#
# Calculating the average and standard error (SE) #
#-------------------------------------------------#

n = dim(new.dfr)[1]

ave.std = vector()

for(i in 1:n){
  vec = as.numeric(new.dfr[i, c("data1", "data2", "data3")])
  ave = round(mean( vec, na.rm = TRUE ), 1)
  std = round(  sd( vec, na.rm = TRUE )/sqrt(3), 2)
  
  ave.std[i] = paste(ave, "\u00B1", std, sep = " ")
}

#-----------------------------------#
# Displaying and saving the results #
#-----------------------------------#

# Displaying #
out.dfr = data.frame(new.dfr$abbreviture, new.dfr$unit, ave.std)
colnames(out.dfr) = c("Abbreviture", "Unit", "Average \u00B1 SE")
cat("Results... \n")

show(out.dfr)

# Saving (csv format) #
write.csv(out.dfr, paste0(wd.path, "Output/","Output_Soil_", file, ".csv"))

