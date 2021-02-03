##=============================================##
## Descriptive Analysis of Meteorological Data ##
##=============================================##

##-------------------------##
## Clean the R environment ##
##-------------------------##

rm(list = ls())

file = "La Victoria - Test"  ## Change it by your file's name

#--------------------------------------------------#
# Loading libraries, work directory and Excel file #
#-------------------------d-------------------------#

# Libraries #
library(openxlsx)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)

# Worrk directory #
DW = getwd()
nDW = nchar(DW) - 7

wd.path = substr(DW, 1, nDW)

# Excel file #
dfr = read.xlsx(xlsxFile = paste0(wd.path, "Data/", file, ".xlsx") , sheet = "Weather", startRow = 2)

#
#dfr$`PAR,.MJm-2day-1.(LGR.S/N:.10224387,.SEN.S/N:.1247977,.LBL:.Periodo.2018-19-Hyo-LV)` = (dfr$`PAR,.uE.(LGR.S/N:.10224387,.SEN.S/N:.1247977,.LBL:.Periodo.2018-19-Hyo-LV)`)*900/(4.6*1000000)
#

xn = names(dfr)[6:length(names(dfr))]
name_final = paste0(1:length(xn), "_", substr(xn, 1, 3))

set_dfr = dfr[1:500, 6:length(names(dfr))]
colnames(set_dfr) = name_final

dfr_zo = zoo(set_dfr)
plot(dfr_zo)

#---------------------#
# Selecting variables #
#---------------------#

# variables' position in character # 
name = names(dfr)
cat("\n Selecting the meteorological variables: 1, 2, ... \n \n")
print(name[6: length(name)])

cat("\n To average (Temperature, Humidity, etc.): \n")
varAVE = readline(prompt = "[enter here]: > ")

cat("\n To sum (Radiation, Rain, etc.):")
varSUM = readline(prompt = "[enter here]: > ")

# Converting from character to numeric #
varAVE = as.numeric(unlist(strsplit(varAVE, "[,]"))) + 5
varSUM = as.numeric(unlist(strsplit(varSUM, "[,]"))) + 5

#-------------------------------#
# Preparing the variables' name #
#-------------------------------#

list.name = name[c(varAVE, varSUM)]
splt.name = strsplit(list.name, "[,]")

var.symb = vector()
var.unit = vector()

for(i in 1:length(list.name)){
  
  var.symb[i] = splt.name[[i]][1]
  var.unit[i] = unlist(strsplit(splt.name[[i]][2], "[.]"))[2]
  
}

# Meteorological variables selected #
met = dfr[, c(2, varAVE, varSUM)]
colnames(met) = c("Date", var.symb)

#-----------------------#
# Ploting some boxplots #
#-----------------------#
plot_met = dfr[, c(varAVE, varSUM)]
colnames(plot_met) = c(var.symb)

meltData = melt(plot_met)
boxplot(data = meltData, value ~ variable)

# Preparing the date-time format #
met$DateTime = convertToDateTime(met$Date, origin = "1900-01-01") # openxlsx library's function

#--------------------#
# Preparing the data #
#--------------------#

# Grouping date-time data by month (g_ym) and day (g_ymd) #
met$DateTimeAsChar = as.character(met$DateTime)

met$gm_ly = substr(met$DateTimeAsChar , 1,  7)
met$gd_ly = substr(met$DateTimeAsChar , 1, 10)

#
m_ly = unique(met$gm_ly)
d_ly = unique(met$gd_ly)


# Matrix to save the data daily (dly) calculated #
col_met_d = length(var.symb) + 2*length(varAVE)
dly = matrix(nrow = length(d_ly), ncol = col_met_d)
n_d = vector(length = length(d_ly))

#------------------------------#
# Creating the variables' name #
#------------------------------#

vname = vector()

# To variables averaged #
for(j in 1:length(varAVE)){

  vname[3*j - 2] = paste0(var.symb[j], "_ave (", var.unit[j], ")")
  vname[3*j - 1] = paste0(var.symb[j], "_min (", var.unit[j], ")")
  vname[3*j    ] = paste0(var.symb[j], "_max (", var.unit[j], ")")

}

# To variables summarized #
for(k in 1:length(varSUM)){
  
  nk = length(varAVE)
  vname[3*nk +k] =  paste0(var.symb[nk + k], "_sum (", var.unit[nk + k], ")")
  
}

# Final variables' name #
vname = c("Date", "ndata", vname)

#-----------------------------#
# Summarizing the data by day #
#-----------------------------#

for(i in 1:length(d_ly)){
  
  # dataframe with data per day #
  xdata = met[met$gd_ly == d_ly[i], ]
  n_d[i] = length(xdata[, 1])
  
  # To average data (varAVE) #
  for(j in 1:length(varAVE)){

    x1 = xdata[, 1 + j]
    
    dly[i, 3*j - 2] = mean(x1, na.rm = TRUE)
    dly[i, 3*j - 1] =  min(x1, na.rm = TRUE)
    dly[i, 3*j    ] =  max(x1, na.rm = TRUE)
    
    vname[3*j - 2]
  }
  
  # To sum data (varSUM) #
  for(k in 1:length(varSUM)){
    
    x2 = xdata[, 1 + length(varAVE) + k] 
    
    dly[i, 3*length(varAVE) + k] =  sum(x2, na.rm = TRUE)
  }
}

dly = round(dly, 1)
out_dly = data.frame(as.Date(d_ly), n_d, dly)
colnames(out_dly) = vname

#-------------------------------#
# Summarizing the data by month #
#-------------------------------#

# Selecting variables to average or sum #
cat("\n Your meteorological data daily is ready... \n \n")

print(head(out_dly))

#
cat("\n From the next list: \n \n")
print(data.frame(variable = vname[3:length(vname)]))
#
cat("\n Select the variables to average by month: \n")

vAVE = readline(prompt = "[enter here]: > ")
vAVE = as.numeric(unlist(strsplit(vAVE, "[,]"))) + 2

# Importing the meteorological data daily #
ydfr = out_dly
ydfr$month = substr(as.character(out_dly$Date), 1, 7)

# Matrix to save the data monthly (mly) calculated #
col_met_m = length(vname) - 2 + length(vAVE)

mly = matrix(nrow = length(m_ly), ncol = col_met_m)
n_m = vector(length = length(m_ly))

v_name = vector(length = col_met_m)

# Summarizing by month #
for(i in 1:length(m_ly)){
  
  # dataframe with data per month #
  ydata = ydfr[ydfr$month == m_ly[i], ]
  n_m[i] = length(ydata[, 1])
  
  # To average data (varAVE) #
  for(j in 1:length(vAVE)){
    
    y1 = ydata[, 2 + j]
    
    mly[i, 2*j - 1] = mean(y1, na.rm = TRUE)
    mly[i, 2*j    ] =   sd(y1, na.rm = TRUE)/sqrt(length(x1))
    
    v_name[2*j - 1] = vname[2 + j]
    v_name[2*j    ] = "se"
 
  }
  
  # To sum data (varSUM) #
  for(k in 1:(col_met_m - 2*length(vAVE))){
    
    y2 = ydata[, 2 + length(vAVE) + k] 
    
    mly[i, 2*length(vAVE) + k] =  sum(y2, na.rm = TRUE)
    
    v_name[2*length(vAVE) + k] = vname[2 + length(vAVE) + k]
  }
}

mly = round(mly, 2)
out_mly = data.frame(m_ly, n_m, mly)
colnames(out_mly) = c("Month", "ndata" ,v_name)

print(out_mly)

#--------------------#
# Saving the results #
#--------------------#

write.csv(out_dly, paste0(wd.path, "Output/","Output_Weather_daily_", file, ".csv"))
write.csv(out_mly, paste0(wd.path, "Output/","Output_Weather_monthly_", file, ".csv"))


# To convert data uE to MJ m-2 dia-1 use: 900/(4.6*1000000)






