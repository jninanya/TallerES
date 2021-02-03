#-------------------------#
# Clean the R environment #
#-------------------------#

rm(list = ls())

FileName = "La Victoria - Test"  ## Change it by your file's name

#------------------------------#
# Libraries and work directory #
#------------------------------#

# Libraries #
library(readxl)
#library(st4gi)
library(ggplot2)
library(factoextra)
library(dplyr)
#library(scales)
library(lme4)
library(lmerTest)

# Work directory #
DW = getwd()
nDW = nchar(DW) - 7

wd.path = substr(DW, 1, nDW)

MainPath <- wd.path

#-------------------------------#
# Loading the excel file's data #
#-------------------------------#



dataYD <- read_xlsx(paste0(MainPath, "Data/", FileName, ".xlsx"), sheet = "Yield")
dataOL <- read_xlsx(paste0(MainPath, "Data/", FileName, ".xlsx"), sheet = "Organolepticas Lab")
dataMN <- read_xlsx(paste0(MainPath, "Data/", FileName, ".xlsx"), sheet = "Minerales")
dataVC <- read_xlsx(paste0(MainPath, "Data/", FileName, ".xlsx"), sheet = "Vitamin C")
dataGA <- read_xlsx(paste0(MainPath, "Data/", FileName, ".xlsx"), sheet = "Glicoalcaloides")
 
dataYD <- as.data.frame(dataYD) # 3 rep
dataOL <- as.data.frame(dataOL) # 
dataMN <- as.data.frame(dataMN) # 3 rep
dataVC <- as.data.frame(dataVC) # 1 rep
dataGA <- as.data.frame(dataGA) # 1 rep

# Relevant breeding traits #
traitYD <- c("Plant_Unif", "Plant_Vigor", "FLOWERING", "PLAHE", "SNPP", "Leng_Stolon", 
             "Tuber_Apper", "Tub_Unif", "Tub_Size", "MTWP", "TTYA", "ATW", "AVDM", "APGL")
traitOL <- c("Textura", "Sabor", "Sab_Extr", "Oscur")
traitMN <- c("FeDW", "ZnDW")
traitVC <- c("ASC_FW")
traitGA <- c("Tgly_FW")

# Data with three replications: YD & MN #
data <- data.frame(dataYD[, traitYD], dataMN[, traitMN])
dataYD_MN <- data.frame(as.factor(dataYD$REP), as.factor(dataYD$INSTN), data)

colnames(dataYD_MN) <- c("REP", "INSTN", traitYD, traitMN)
nameT <- colnames(dataYD_MN)

#-------------------------------------------#
# checks that every replication is complete #
#-------------------------------------------#

#for(i in 3:length(nameT)){
#  check.freq(nameT[i], "INSTN", rep = "REP", dfr = dataYD_MN)
#}

#-------------------------------------#
# Boxplots to compare the replications #
#-------------------------------------#

x1 <- dataYD_MN

# Creating a new directory to save the boxplots #
dir.create(paste0(MainPath, "Output/Images_BoxPlots/"), showWarnings = FALSE)

imagesDir <- paste0(MainPath, "Output/Images_BoxPlots/")
png(file.path(imagesDir, "Boxplot%02d.png"), width = 800, height = 500, units = "px", pointsize = 12)

for(i in 3:length(nameT)){
  
  x1$trait <- dataYD_MN[, nameT[i]]
  boxplot(trait ~ REP, ylab = nameT[i], main = paste0(nameT[i], " per REP"), data = x1)
}

dev.off()

#-----------------------------------------------------#
# Histograms to take a look whether data looks normal #
#-----------------------------------------------------#

x2 <- dataYD_MN

# Creating a new directory to save the histograms #
dir.create(paste0(MainPath, "Output/Images_Histograms/"), showWarnings = FALSE)

# Histograms per trait #
imagesDir1 <- paste0(MainPath, "Output/Images_Histograms/")
png(file.path(imagesDir1, "Histograms%02d.png"), width = 800, height = 500, units = "px", pointsize = 12)

for(i in 3:length(nameT)){
  
  x2$trait <- dataYD_MN[, nameT[i]]
  hist(x2$trait, main = paste("Histogram of" , nameT[i]))
}

dev.off()

#-----------------------#
# Mix model predictions #
#-----------------------#

x3 <- dataYD_MN

blups <- matrix(nrow = length(unique(x3$INSTN)), ncol = length(nameT))
means <- matrix(nrow = length(unique(x3$INSTN)), ncol = length(nameT))

for(k in 3:length(nameT)){
  
  x3$trait <- dataYD_MN[, nameT[k]]
  
  model <- lmer(trait ~ -1 + REP + (1|INSTN), data = x3)
  
  summary(model)
  overall.mean <- mean(fixef(model))
  
  blups.genotypes <- overall.mean + ranef(model)$INSTN
  
  blups[, k] = blups.genotypes$`(Intercept)`
  
  
  xmean = x3 %>%
    group_by(INSTN) %>%
    summarise_at(vars(trait), list(mean = ~mean(.,na.rm = T)))
  
  means[, k] = xmean$mean
}

blups = as.data.frame(blups)
blups[, 1] = 1:length(unique(x3$INSTN))
blups[, 2] = rownames(blups.genotypes)

means = as.data.frame(means)
means[, 1] = 1:length(unique(x3$INSTN))
means[, 2] = rownames(blups.genotypes)

colnames(blups) = c("id", nameT[2:length(nameT)])
colnames(means) = c("id", nameT[2:length(nameT)])

Blups = blups[order(blups$INSTN),]
Means = means[order(means$INSTN),]

write.csv(Blups, paste0(MainPath, "Output/Output_BLUPs_", FileName, ".csv"))
write.csv(Means, paste0(MainPath, "Output/Output_MEANs_", FileName, ".csv"))

#------------------------------#
# Other traits: Organolepticas #
#------------------------------#

xTextura = dataOL %>%
  group_by(INSTN) %>%
  summarise_at(vars(Textura), list(mean = ~mean(.,na.rm = T)))

xSabor = dataOL %>%
  group_by(INSTN) %>%
  summarise_at(vars(Sabor), list(mean = ~mean(.,na.rm = T)))

xSab_Extr = dataOL %>%
  group_by(INSTN) %>%
  summarise_at(vars(Sab_Extr), list(mean = ~mean(.,na.rm = T)))

xOscur = dataOL %>%
  group_by(INSTN) %>%
  summarise_at(vars(Oscur), list(mean = ~mean(.,na.rm = T)))

OrgLab = data.frame(xSab_Extr$INSTN, xTextura$mean, xSabor$mean, xSab_Extr$mean, xOscur$mean)
colnames(OrgLab) = c("INSTN", traitOL)

OrgLab = OrgLab[order(OrgLab$INSTN),]

#------------------------------#
# Other traits: VitC & Glicoal #
#------------------------------#

xvit = dataVC[, c("INSTN", "ASC_FW")]
xgli = dataGA[, c("INSTN", "Tgly_FW")] 

xv = xvit[order(xvit$INSTN),]
xg = xgli[order(xgli$INSTN),]

RestTraits = data.frame(OrgLab, xv$ASC_FW, xg$Tgly_FW)
colnames(RestTraits) = c("INSTN", traitOL, traitVC, traitGA)

write.csv(RestTraits, paste0(MainPath, "Output/Output_RestTraits_", FileName, ".csv"))

