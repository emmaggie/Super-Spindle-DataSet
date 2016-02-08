#http://www.statmethods.net/advstats/mds.html
#https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref1
#plot.1
#http://www.statmethods.net/graphs/scatterplot.html
#install.packages('AppliedPredictiveModeling')
library(AppliedPredictiveModeling)

library(ggplot2)
#library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(scatterplot3d)
#install.packages('scatterplot3d')
library(caret)
library(MASS)

library(corrplot)
setwd('/Volumes/Seagate_RED/A_Heald_Lab/A_My_papers/A_TECH_PAPER/Super-Spindle-DataSet/Code')

###############################################################################################
###############################################################################################
#CORRELATIONS
###############################################################################################
###############################################################################################
corr.all=cor(df_cont.cp)
load('../Data/TPX2_correlations_all.Robj')
load('../Data/TPX2_df_cont.cp.Robj')
head(corr.all)
col2 <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")))
#corr.all

#pdf('../plots/TPX2_rcorr_plot.pdf')#,width=2000, height=2000)
par(cex=0.8)
corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="black",tl.cex=0.5)
#dev.off()

#pdf('../plots/TPX2_rcorr_plot.pdf')#,width=2000, height=2000)
par(cex=0.8)
par(lwd=0.2)
corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="black",tl.cex=0.5,rect.lwd=0,outline=FALSE,addrect=0,addgrid.col = 'white')

corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="#053061",tl.cex=0.5)
corrplot(corr.all, method='color',col=col2(dim(df_cont.cp)[2]),tl.col="#053061",tl.cex=0.5)

?corrplot

################################################################################################
################################################################################################
#         SINGLE plots: AWG
################################################################################################
################################################################################################

df=read.csv('../../A_PAPER2_PIPELINE/TPX2/MyExpt_FilteredSpindles.csv',header = TRUE, stringsAsFactors=FALSE)

plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=1/Math_AspectRatio,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()#+theme_update(panel.grid.minor=element_line(size=0.5))#+geom_jitter(aes(factor(Y_binary),y=1/Math_AspectRatio))
#ggsave(file='../plot.1s/TPX2_aspect_violin.pdf')
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=1/Math_AspectRatio,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()#+geom_jitter(aes(factor(Y_binary),y=1/Math_AspectRatio))
#ggsave(file='../plot.1s/TPX2_aspect_boxplot.1.pdf', useDingbats=FALSE)

plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_Eccentricity,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_eccentricity_violin.pdf')
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_Eccentricity,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_eccentricity_boxplot.1.pdf',useDingbats=FALSE)


plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_MajorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_length_violin.pdf',useDingbats=FALSE)
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_MajorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_length_boxplot.1.pdf',useDingbats=FALSE)


plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_MinorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_width_violin.pdf',useDingbats=FALSE)
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_MinorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_width_boxplot.1.pdf',useDingbats=FALSE)

plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=Intensity_IntegratedIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_IntegratedIntensityRh_violin.pdf',useDingbats=FALSE)
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=Intensity_IntegratedIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_IntegratedIntensityRh_boxplot.1.pdf',useDingbats=FALSE)

plot.1=ggplot(data=df)
plot.1+geom_violin(aes(factor(Metadata_Treatment),y=Intensity_MedianIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_Intensity_MedianIntensityRh_violin.pdf',useDingbats=FALSE)
plot.1=ggplot(data=df)
plot.1+geom_boxplot(aes(factor(Metadata_Treatment),y=Intensity_MedianIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../plot.1s/TPX2_Intensity_MedianIntensityRh_boxplot.1.pdf',useDingbats=FALSE)



################################################################################################
################################################################################################
#         SINGLE plots: FEATURE_SELECTION
################################################################################################
################################################################################################

#this is the name of this data frame in the notebook
df_cont=read.csv('../Data/TPX2_df_with_bin_Y.csv',header = TRUE, stringsAsFactors=FALSE)


library(ggplot2)
df_cont$Metadata_Treatment = as.factor(df_cont$Metadata_Treatment)

#5
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=1/Math_AspectRatio ,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
ggsave('../Plots/TPX2_violin_Math_AspectRatio_5.pdf')

#13
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=AreaShape_Eccentricity ,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_AreaShape_Eccentricity_13.pdf')

#15
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=Intensity_MeanIntensity_Rhodamine,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_Intensity_MeanIntensity_Rhodamine_15.pdf')

#20
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=log(AreaShape_Compactness),fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_log_AreaShape_Compactness_20.pdf')

#32 AreaShape_Extent
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=AreaShape_Extent,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_log_AreaShape_Extent_32.pdf')

#47 Intensity_MedianIntensity_DNA
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=Intensity_MedianIntensity_DNA,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_Intensity_MedianIntensity_DNA_47.pdf')

#52 AreaShape_MeanRadius
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=AreaShape_MeanRadius,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_AreaShape_MeanRadius_52.pdf')

#55 Mean_FilteredChromatin_AreaShape_MajorAxisLength
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=log(Mean_FilteredChromatin_AreaShape_MajorAxisLength),fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_Mean_FilteredChromatin_AreaShape_MajorAxisLength_55.pdf')

#63 Mean_FilteredChromatin_Intensity_IntegratedIntensity_DNA
plot.1=ggplot()
plot.1 = plot.1+geom_violin(data=df_cont,aes(x=Metadata_Treatment,y=Mean_FilteredChromatin_Intensity_IntegratedIntensity_DNA,fill=factor(Metadata_Treatment)))
plot.1+theme(legend.position='none')+scale_fill_manual(values=c('#7F7F7F','#179B33'))
#ggsave('../Plots/TPX2_violin_Mean_FilteredChromatin_Intensity_IntegratedIntensity_DNA_63.pdf')


load('../Data/TPX2_features_PCA.Robj')
ggbiplot(df_cont_no_Y_trans_no_null_var_PCA.3.params , obs.scale = 1, var.scale = 1, groups=as.character(Y_binary), ellipse=TRUE, circle=FALSE, var.axes=FALSE, ellipse.prob = 0.68, varname.abbrev=TRUE, varname.size = 0, alpha=0.5)+scale_color_manual(values=c('#7F7F7F','#179B33'))
#ggsave(file='../Plots//TPX2_features_ggbiplot_PCA.pdf',useDingbats=FALSE)


################################################################################################
################################################################################################
#         MDS plot.1S
################################################################################################
################################################################################################

df_cont=read.csv('../Data/TPX2_no_Y_transf.csv',header = TRUE, stringsAsFactors=FALSE)
df_cont_w_Y = read.csv('../Data/TPX2_df_with_bin_Y.csv', header = TRUE, stringsAsFactors=FALSE)

dim(df_cont) #one dimension too many!
head(df_cont)

names(df_cont)
names(df_cont_w_Y)

df_cont=df_cont[,!names(df_cont) %in% c('X')]
df_cont_w_Y=df_cont_w_Y[,!names(df_cont_w_Y) %in% c('X')]

names(df_cont)
names(df_cont_w_Y)

###############################################
#         PCA - ALL - LOG-TRANSFORMED
###############################################
#min(abs(df_cont[,1:174]))
#df_cont.log=log(df_cont[,1:174])
#df_cont.sqrt=sqrt(df_cont[,1:174])

Target=df_cont_w_Y[,'Metadata_Treatment']

#df_cont.pca=prcomp(df_cont.log, center = TRUE, scale=TRUE, )



##########
#PROBLEM 1: - some columns have 0 variance
##########
#names(apply(df_cont.log, 2, var, na.rm=TRUE)[which(apply(df_cont.log, 2, var, na.rm=TRUE)==0)])#AreaShape_EulerNumber
#new
names(apply(df_cont, 2, var, na.rm=TRUE)[which(apply(df_cont, 2, var, na.rm=TRUE)==0)])#AreaShape_EulerNumber


#df_cont.log.no_null_var=df_cont.log[,names(apply(df_cont.log, 2, var, na.rm=TRUE)[which(apply(df_cont.log, 2, var, na.rm=TRUE)!=0)])]
#new
df_cont.no_null_var=df_cont[,names(apply(df_cont, 2, var, na.rm=TRUE)[which(apply(df_cont, 2, var, na.rm=TRUE)!=0)])]

##########
#PROBLEM 2: some variables have NaN values
##########

#unique(which(is.na(df_cont.log.no_null_var)==TRUE, arr.ind = TRUE)[,2])
#13 and 103


#names(df_cont.log.no_null_var)[13] #"AreaShape_Orientation"
#names(df_cont.log.no_null_var)[103] #"Mean_FilteredChromatin_AreaShape_Orientation"
hist(df_cont[,"AreaShape_Orientation"])
hist(df_cont[,"AreaShape_Orientation"]+180)
min(df_cont[,"AreaShape_Orientation"]+180)
max(df_cont[,"AreaShape_Orientation"]+180)

hist(df_cont[,"Mean_FilteredChromatin_AreaShape_Orientation"])

df_cont[,"AreaShape_Orientation"]=df_cont[,"AreaShape_Orientation"]+180
df_cont[,"Mean_FilteredChromatin_AreaShape_Orientation"]=df_cont[,"Mean_FilteredChromatin_AreaShape_Orientation"]+180
df_cont.log=log(df_cont[,1:174])
df_cont.sqrt=sqrt(df_cont[,1:174])

df_cont.log.no_null_var=df_cont.log[,names(apply(df_cont.log, 2, var, na.rm=TRUE)[which(apply(df_cont.log, 2, var, na.rm=TRUE)!=0)])]

unique(which(is.na(df_cont.log.no_null_var)==TRUE, arr.ind = TRUE)[,2])

##########FINAL: ALL DATA
#df_cont.pca=prcomp(df_cont.log.no_null_var, center = TRUE, scale=TRUE)
df_cont.pca.2=prcomp(df_cont.no_null_var, center = TRUE, scale=TRUE)

str(df_cont.pca)
#plot(df_cont.pca, type='l')
plot(df_cont.pca.2, type='l')

summary(df_cont.pca)

?ggbiplot
ggbiplot(df_cont.pca.2, obs.scale = 1, var.scale = 1, groups=as.character(Target), ellipse=TRUE, circle=FALSE, var.axes=FALSE, ellipse.prob = 0.68, varname.abbrev=TRUE, varname.size = 0, alpha=0.5)+scale_color_manual(values=c('#7F7F7F','#179B33'))
#g <- g + scale_color_discrete(name = '')
#ggsave(file='../plot.1s/TPX2_ggbiplot.1_PCA.pdf',useDingbats=FALSE)
#ggsave(file='../plot.1s/TPX2_ggbiplot.1_PCAv2.pdf',useDingbats=FALSE)

###BELOW - NOT HELPFUL!!!!
#not to helpful for such comlex data set
require(ggplot2)
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(df_cont.pca$rotation, .names = row.names(df_cont.pca$rotation))
p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) + coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")

###############################################
#         PCA - TPX2 univariate - LOG-TRANSFORMED
###############################################
df_cont.log.univariate=df_cont.log[,c('Math_AspectRatio','AreaShape_Eccentricity','AreaShape_MajorAxisLength','AreaShape_MinorAxisLength','Intensity_IntegratedIntensity_Rhodamine','Intensity_MedianIntensity_Rhodamine')]
df_cont.log.univariate.pca=prcomp(df_cont.log.univariate, center = TRUE, scale=TRUE )
dim(df_cont.log.univariate.pca)

ggbiplot(df_cont.log.univariate.pca, obs.scale = 1, var.scale = 1, groups=as.character(Target), ellipse=TRUE, circle=TRUE, var.axes=TRUE, ellipse.prob = 0.68, varname.size = 3, alpha=0.5)+scale_color_manual(values=c('#7F7F7F','#179B33'))
#ggsave(file='../plot.1s/TPX2_ggbiplot.1_PCA_univariate.pdf',useDingbats=FALSE)
#ggsave(file='../plot.1s/TPX2_ggbiplot.1_PCA_univariatev2.pdf',useDingbats=FALSE)

###############################################
#         PCA - TPX2 multivariate - CARET
###############################################
#https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref1



require(caret)
#will also not handle zero-var columns
df_cont.no_null_var=df_cont[,names(apply(df_cont[,1:174], 2, var, na.rm=TRUE)[which(apply(df_cont[,1:174], 2, var, na.rm=TRUE)!=0)])]

dim(df_cont[,names(apply(df_cont[,1:174], 2, var, na.rm=TRUE)[which(apply(df_cont[,1:174], 2, var, na.rm=TRUE)!=0)])]) #4556  173
names(df_cont.no_null_var)

trans=preProcess(df_cont.no_null_var, method=c("BoxCox",'center','scale','pca'))
PC=predict(trans,df_cont.no_null_var)
dim(PC)
#write.csv(PC, '../Data/TPX2_PC_caret.csv')

head(PC)
#LOADINGS:
trans$rotation

names(PC.target)
PC.target=cbind(PC,Target=as.factor(Target))
class(PC.target$Target)
plot.1=ggplot(data=PC.target)+geom_point(aes(x=PC1,y=PC2, colour=Target), alpha=0.7)+scale_color_manual(values=c('#7F7F7F','#179B33'))
plot.1
#ggsave(file='../plot.1s/TPX2_ggbiplot.1_PCA_multivariate_caret_v2.pdf',useDingbats=FALSE)



library(psych)
#this once is transformed already (equivalent to df_cont_no_Y_trans_no_null_var in the NB)
?fa.parallel
fa.parallel(df_cont.no_null_var)
