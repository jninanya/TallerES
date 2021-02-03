##================================##
## Descriptive Analysis of traits ##
##================================##

##-------------------------##
## Clean the R environment ##
##-------------------------##

rm(list = ls())

file = "La Victoria - Test"  ## Change it by your file's name

NCP = 3 # Number of principal components

#--------------------------------------------------#
# Loading libraries, work directory and Excel file #
#--------------------------------------------------#

# Libraries #
library(openxlsx)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
library(psych)

library(FactoMineR)
library(missMDA)

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot")

#install.packages("digest")

library(ggcorrplot)

# Worrk directory #
DW = getwd()
nDW = nchar(DW) - 7

wd.path = substr(DW, 1, nDW)

# Excel file #
dfr1 = read.csv(paste0(wd.path, "Output/", "Output_BLUPs_", file, ".csv"))
dfr2 = read.csv(paste0(wd.path, "Output/", "Output_RestTraits_", file, ".csv"))


dat = data.frame(dfr1[, 3:length(names(dfr1))], dfr2[,3:length(names(dfr2))])

# summarizing #

inst = dat$INST

dfr_cor = dat[, 2:length(names(dat))]

name = names(dat)

mcor = corr.test(dfr_cor)
out_cor = data.frame(mcor$r)
colnames(out_cor) = tolower(name[2:length(name)])
rownames(out_cor) = tolower(name[2:length(name)])

#--------------------#
# Saving the results #
#--------------------#

write.csv(out_cor, paste0(wd.path, "Output/","Output_TraitsCorrelations_", file, ".csv"))

pl1 = ggcorrplot(mcor$r, outline.color = "white", p.mat = mcor$p, 
           sig.level = 0.05, pch = "*", pch.cex = 5, pch.col = "gray", 
           hc.order = F, type = "lower",  ggtheme = ggplot2::theme_gray)

pl2 = ggcorrplot(mcor$r, outline.color = "white", p.mat = mcor$p, 
                 sig.level = 0.05, pch = "*", pch.cex = 5, pch.col = "gray", 
                 hc.order = F,  ggtheme = ggplot2::theme_gray)

dir.create(paste0(wd.path, "Output/Images_Corr&PCA/"), showWarnings = FALSE)

imagesDir <- paste0(wd.path, "Output/Images_Corr&PCA/")
png(file.path(imagesDir, "CorrelationsTraits_lower.png"), width = 800, height = 500, units = "px", pointsize = 12)
plot(pl1)
dev.off()

png(file.path(imagesDir, "CorrelationsTraits.png"), width = 800, height = 500, units = "px", pointsize = 12)
plot(pl2)
dev.off()

#
#coef.corr = mcor$r
#coef.d = (1 - coef.corr^2)
#d = as.dist(as.matrix(coef.d))
#
#tr = spantree(d)
#plot(tr, cmdscale(d), type = "t")
#
#plot(tr, type = "t")
#depths <- spandepth(tr)
#plot(tr, type = "t", label = depths)
#cl <- as.hclust(tr)
#plot(cl)


#-------------------------------------#
# Analisis de Componentes Principales # 
#-------------------------------------#

#nb = estim_ncpPCA(dfr_cor, ncp.max = 5)
#res.comp = imputePCA(dat, ncp = 1)
#dat.comp = data.frame(res.comp$completeObs)

png(file.path(imagesDir, "PCA_BreedingTraits.png"), width = 800, height = 500, units = "px", pointsize = 12)
dat.comp = dfr_cor
res.pca = PCA(dat.comp, ncp = NCP, scale.unit = TRUE , graph = TRUE)
dev.off()
#dfr.pca = prcomp(dat.comp, center = TRUE, scale = TRUE)

pca.s = summary(res.pca)

res.hcpc = HCPC(res.pca)

png(file.path(imagesDir, "PCA_Cluster.png"), width = 800, height = 500, units = "px", pointsize = 12)
dfr.out = data.frame("INSTN" = inst, res.hcpc$data.clust)

write.csv(dfr.out, paste0(wd.path, "Output/","Output_PCAAnalysis_", file, ".csv"))
#write.csv(dfr.out, "pca_ni.csv")
dev.off()

capture.output(summary(res.pca), file = paste0(wd.path, "Output/","Output_SummaryPCA_", file, ".doc"))

print(summary(res.pca))
