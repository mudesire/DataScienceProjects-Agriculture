rm(list = ls(all.names = TRUE)) 
######## Used library//co
library('readxl')
library('ggcorrplot')
library('ggplot2')
library('factoextra')
library('corrplot')
library(pheatmap)
library("RColorBrewer")
library("gplots")

#######

#data3fielddata

data1="x.xlsx"
data2="y.xlsx"
data3="z.xlsx"


# 
# Set data source to process
data=data3

# 
# Select column 7:28
dataset <- read_excel(data)
newdata <- dataset[c(1,7:NCOL(dataset))] 
newdata <- data.frame(newdata[c(-1)])

# 
#Ignore rows with missing values

sum(is.na(newdata)) #  There is a missing value check please
newdata=na.omit(newdata)
which(is.na(newdata) ==TRUE, arr.ind = T)

# 
# Compute correlation
newdatacor= cor(newdata,method = c("pearson"))
newdatacor=round(newdatacor,2)
title="Pearson correlation"

pg=ggcorrplot(newdatacor,hc.order = TRUE, type = "lower", legend.title =title )
pg=pg+theme(axis.title=element_text(size=2))
print(pg)
ggsave(file="newdatacor.png")
ggsave(file="newdatacor.pdf")
dev.off()
#
# Save graph
ph=pheatmap(newdatacor)
# creates a 5 x 5 inch image
png("newdatapcapheatmap.png",    # create PNG for the heat map        
    width = 5*300+50,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 4)        # smaller font size
print(ph)
dev.off()
pdf(file ="newdatapcapheatmap.pdf")        # smaller font size
print(ph)
dev.off()

#reformulate
#https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/)


#
#Customizing and plotting the heat map
# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,0,length=100),  # for red
               seq(0.01,0.8,length=100),           # for yellow
               seq(0.81,1,length=100))             # for green

# creates a 5 x 5 inch image
png("newdatapcapheatmap2.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size
heatmap.2(newdatacor,
          #cellnote = newdatacor,  # same data set for cell labels
          main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering

dev.off()               # close the PNG device
#
# PCA
newdatapca = prcomp(newdata, center = TRUE, scale = TRUE)
summary(newdatapca)
write.csv(data.frame(newdatapca$x) ,'newdatapca.csv')
#
# 

p3=fviz_pca_ind(newdatapca, label="none", habillage=newdata$Row,addEllipses=TRUE, 
                ellipse.level=0.90)+ scale_color_brewer(palette="Dark2") +
                    theme_minimal()
p3
ggsave(file="newdatapcap3.png")
ggsave(file="newdatapcap3.pdf")
dev.off()



# Default plot
pv=fviz_pca_biplot(newdatapca, geom = "point", col.ind="contrib")+
             scale_color_gradient2(low="white", mid="blue",high="red")+
                theme_minimal()
print(pv)
ggsave(file="newdatapcapv.png")
ggsave(file="newdatapcapv.pdf")
dev.off()