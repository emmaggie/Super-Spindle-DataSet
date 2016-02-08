#http://www.statmethods.net/advstats/mds.html
#https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref1
#PLOT
#http://www.statmethods.net/graphs/scatterplot.html

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
#corr.all=cor(df_cont.cp)
load('../Data/TPX2_correlations_all.Robj')
load('../Data/TPX2_df_cont.cp.Robj')
head(corr.all)
col2 <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")))
#corr.all

pdf('../Plots/TPX2_rcorr_plot.pdf')#,width=2000, height=2000)
par(cex=0.8)
corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="black",tl.cex=0.5)
dev.off()

pdf('../Plots/TPX2_rcorr_plot.pdf')#,width=2000, height=2000)
par(cex=0.8)
par(lwd=0.2)
corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="black",tl.cex=0.5,rect.lwd=0,outline=FALSE,addrect=0,addgrid.col = 'white')

corrplot(corr.all, method='ellipse',col=col2(dim(df_cont.cp)[2]),tl.col="#053061",tl.cex=0.5)
corrplot(corr.all, method='color',col=col2(dim(df_cont.cp)[2]),tl.col="#053061",tl.cex=0.5)

?corrplot

################################################################################################
################################################################################################
#         SINGLE PLOTS
################################################################################################
################################################################################################

df=read.csv('../../A_PAPER2_PIPELINE/TPX2/MyExpt_FilteredSpindles.csv',header = TRUE, stringsAsFactors=FALSE)

plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=1/Math_AspectRatio,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()#+theme_update(panel.grid.minor=element_line(size=0.5))#+geom_jitter(aes(factor(Y_binary),y=1/Math_AspectRatio))
#ggsave(file='../Plots/TPX2_aspect_violin.pdf')
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=1/Math_AspectRatio,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()#+geom_jitter(aes(factor(Y_binary),y=1/Math_AspectRatio))
#ggsave(file='../Plots/TPX2_aspect_boxplot.pdf', useDingbats=FALSE)

plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_Eccentricity,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_eccentricity_violin.pdf')
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_Eccentricity,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_eccentricity_boxplot.pdf',useDingbats=FALSE)


plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_MajorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_length_violin.pdf',useDingbats=FALSE)
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_MajorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_length_boxplot.pdf',useDingbats=FALSE)


plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=AreaShape_MinorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_width_violin.pdf',useDingbats=FALSE)
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=AreaShape_MinorAxisLength,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_width_boxplot.pdf',useDingbats=FALSE)

plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=Intensity_IntegratedIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_IntegratedIntensityRh_violin.pdf',useDingbats=FALSE)
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=Intensity_IntegratedIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_IntegratedIntensityRh_boxplot.pdf',useDingbats=FALSE)

plot=ggplot(data=df)
plot+geom_violin(aes(factor(Metadata_Treatment),y=Intensity_MedianIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_Intensity_MedianIntensityRh_violin.pdf',useDingbats=FALSE)
plot=ggplot(data=df)
plot+geom_boxplot(aes(factor(Metadata_Treatment),y=Intensity_MedianIntensity_Rhodamine,fill=factor(Metadata_Treatment)))+scale_fill_manual(values=c('#7F7F7F','#179B33'))+theme_bw()
#ggsave(file='../Plots/TPX2_Intensity_MedianIntensityRh_boxplot.pdf',useDingbats=FALSE)

################################################################################################
################################################################################################
#         MDS PLOTS
################################################################################################
################################################################################################

df_cont=read.csv('../Data/TPX2_df_with_Zernike.csv',header = TRUE, stringsAsFactors=FALSE)
names(df_cont)
df_cont=df_cont[,!names(df_cont) %in% c('X')]

names(df_cont[,1:174])

###############################################
#         PCA - ALL - LOG-TRANSFORMED
###############################################
min(abs(df_cont[,1:174]))
df_cont.log=log(df_cont[,1:174])
df_cont.sqrt=sqrt(df_cont[,1:174])

Y=df_cont[,175]
Target=df[,'Metadata_Treatment']

df_cont.pca=prcomp(df_cont.log, center = TRUE, scale=TRUE, )
##########
#PROBLEM 1: - some columns have 0 variance
##########
names(apply(df_cont.log, 2, var, na.rm=TRUE)[which(apply(df_cont.log, 2, var, na.rm=TRUE)==0)])#AreaShape_EulerNumber

df_cont.log.no_null_var=df_cont.log[,names(apply(df_cont.log, 2, var, na.rm=TRUE)[which(apply(df_cont.log, 2, var, na.rm=TRUE)!=0)])]
##########
#PROBLEM 2: some variables have NaN values
##########

unique(which(is.na(df_cont.log.no_null_var)==TRUE, arr.ind = TRUE)[,2])
#13 and 103
names(df_cont.log.no_null_var)[13] #"AreaShape_Orientation"
names(df_cont.log.no_null_var)[103] #"Mean_FilteredChromatin_AreaShape_Orientation"
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
df_cont.pca=prcomp(df_cont.log.no_null_var, center = TRUE, scale=TRUE)
str(df_cont.pca)
plot(df_cont.pca, type='l')
summary(df_cont.pca)

?ggbiplot
ggbiplot(df_cont.pca, obs.scale = 1, var.scale = 1, groups=as.character(Target), ellipse=TRUE, circle=FALSE, var.axes=FALSE, ellipse.prob = 0.68, varname.abbrev=TRUE, varname.size = 0, alpha=0.5)+scale_color_manual(values=c('#7F7F7F','#179B33'))
#g <- g + scale_color_discrete(name = '')
#ggsave(file='../Plots/TPX2_ggbiplot_PCA.pdf',useDingbats=FALSE)
#ggsave(file='../Plots/TPX2_ggbiplot_PCAv2.pdf',useDingbats=FALSE)

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
#ggsave(file='../Plots/TPX2_ggbiplot_PCA_univariate.pdf',useDingbats=FALSE)
#ggsave(file='../Plots/TPX2_ggbiplot_PCA_univariatev2.pdf',useDingbats=FALSE)

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
ggplot(data=PC.target)+geom_point(aes(x=PC1,y=PC2, colour=Target), alpha=0.7)+scale_color_manual(values=c('#7F7F7F','#179B33'))
#ggsave(file='../Plots/TPX2_ggbiplot_PCA_multivariate_caret_v2.pdf',useDingbats=FALSE)







##############################################################################################
##############################################################################################
#YOU CAN SKIP THE PART BELOW: 
##############################################################################################
##############################################################################################
###############################################
#         MDS - Euclidian - not trasnformed
###############################################
#http://www.statmethods.net/advstats/mds.html

d=dist(df_cont[,1:174])

fit.2=cmdscale(d, eig=TRUE, k=2)
fit.3=cmdscale(d, eig=TRUE, k=3)

x.2=fit.2$points[,1]
y.2=fit.2$points[,2]
MDS.2D=as.data.frame(cbind(x.2, y.2, Target))
unique(Target)
p=ggplot(data=MDS.2D)
p+geom_point(aes(x=x.2, y=y.2, colour=as.factor(Target)), alpha=0.5)+scale_color_manual(values=c('#7F7F7F','#179B33'))

x.3=fit.3$points[,1]
y.3=fit.3$points[,2]
z.3=fit.3$points[,3]
MDS.3D=as.data.frame(cbind(x.3, y.3,z.3, Target))
?scatterplot3d
par(cex=0.5)
scatterplot3d(MDS.3D$x.3, MDS.3D$y.3, MDS.3D$z.3, pch=16,color=ifelse(MDS.3D$Target==302,'#179B33','#7F7F7F'), angle=40)
scatterplot3d(MDS.3D$x.3, MDS.3D$y.3, MDS.3D$z.3, pch=16,color=ifelse(MDS.3D$Target==302,'#179B33','#7F7F7F'), angle=-40)
scatterplot3d(MDS.3D$x.3, MDS.3D$y.3, MDS.3D$z.3, pch=16,color=ifelse(MDS.3D$Target==302,'#179B33','#7F7F7F'), angle=-80)

###############################################
#         MDS - Euclidian - log trasnformed
###############################################
#http://www.statmethods.net/advstats/mds.html

d.log=dist(df_cont.log.no_null_var)
fit.log.2 = cmdscale(d.log, eig=TRUE, k=2)
fit.log.3 = cmdscale(d.log, eig = TRUE, k=3)

x.log.3=fit.log.3$points[,1]
y.log.3=fit.log.3$points[,2]
z.log.3=fit.log.3$points[,3]

MDS.log.3D=as.data.frame(cbind(x.log.3, y.log.3,z.log.3, Target))
scatterplot3d(MDS.log.3D$x.log.3, MDS.log.3D$y.log.3, MDS.log.3D$z.log.3, pch=16,color=ifelse(MDS.log.3D$Target==302,'#179B33','#7F7F7F'), angle=40)

scatterplot3d(MDS.log.3D$x.log.3, MDS.log.3D$y.log.3, MDS.log.3D$z.log.3, pch=16,color=ifelse(MDS.log.3D$Target==302,'#179B33','#7F7F7F'), angle=60)

scatterplot3d(MDS.log.3D$x.log.3, MDS.log.3D$y.log.3, MDS.log.3D$z.log.3, pch=16,color=ifelse(MDS.log.3D$Target==302,'#179B33','#7F7F7F'), angle=80)

###############################################
#         MDS - non-parametric
###############################################
#http://www.statmethods.net/advstats/mds.html
library(MASS)
d.log=dist(df_cont.log.no_null_var)
fit.log.2=isoMDS(d.log, k=3)
x.3=fit.log.2$points[,1]
y.3=fit.log.2$points[,2]
z.3=fit.log.2$points[,3]
MDS.log.3D=as.data.frame(cbind(x.3, y.3, z.3, Target))

scatterplot3d(MDS.log.3D$x.3, MDS.log.3D$y.3, MDS.log.3D$z.3, pch=16,color=ifelse(MDS.log.3D$Target==302,'#179B33','#7F7F7F'), angle=60)


#library(rgl)
#plot3d(MDS.log.3D$x.3, MDS.log.3D$y.3, MDS.log.3D$z.3, pch=16,col=ifelse(MDS.log.3D$Target==302,'#179B33','#7F7F7F'))

#looks like this actually partitions the points

#http://cran.r-project.org/web/packages/SensoMineR/index.html
#http://cran.r-project.org/web/packages/smacof/index.html

