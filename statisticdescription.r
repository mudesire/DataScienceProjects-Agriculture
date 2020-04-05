rm(list = ls(all.names = TRUE)) 
######## Used library//co
library(readxl)
library(ggcorrplot)
library(ggplot2)
library(factoextra)
library(corrplot)
library(pheatmap)
library(RColorBrewer)
library(gplots)
library(psych)
library(FactoMineR)
#######

# 
# Set data source to process
data="NEW ALL DATA.xlsx"
#download.file(url, data)
# 
# Select column 7:28
dataset <- read_excel(data)
newdata <- dataset[c(1,7:NCOL(dataset))] 
newdata <- data.frame(newdata[c(-1)])

# 
#Ignore rows with missing values
options(digits=2)
sum(is.na(newdata)) #  There is a missing value check please
newdata=na.omit(newdata)
# 
newdatapca=PCA(newdata, scale.unit = TRUE, graph = FALSE)
pcaviz=fviz_eig(newdatapca, choice = c("variance"),addlabels = TRUE,hjust = -0.3, ylim = c(0, 30))+labs(title = "Principal Components % of Variances",
                                                               x = "Principal Components", y = "% of variances")
pcaviz


png("fviz_eig.png",    # create PNG for the heat map        
    width = 8*300,        # 5 x 300 pixels
    height = 4*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size
pcaviz
dev.off()
pdf(file ="fviz_eig.pdf")        # smaller font size
print(pcaviz)
dev.off()
#3 premier fonts 
#22.9+13.8+11.3=48

#Contributions of variables to PCs
print(newdatapca$call$ecart.type)

#"$call$centre"     "mean of the variables"              
#"$call$ecart.type" "standard error of the variables"
#"$var$contrib"     "contributions of the variables"
#"$var$cor"         "correlations variables - dimensions"


var_contrib= data.frame(newdatapca$var$contrib)
colnames(var_contrib)=c("PC1","PC2","PC3","PC4","PC5")
call_meansem= data.frame(newdatapca$call$centre,newdatapca$call$ecart.type)
colnames(call_meansem)=c("Mean","SEM") 

newdatadescribe=describe(newdata)
newdatadescribe=data.frame(newdatadescribe$min,newdatadescribe$max,newdatadescribe$range)
colnames(newdatadescribe)=c("Min","Max","Range") 


statisticdescription=data.frame(call_meansem,newdatadescribe,var_contrib) 
write.csv((statisticdescription), 'statisticdescription.csv')


#reference:
#  http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
#  https://www.datacamp.com/community/tutorials/pca-analysis-r


