?write.csv
library(HH)
setwd('/Volumes/Seagate_RED/A_Heald_Lab/A_My_papers/A_TECH_PAPER/Super-Spindle-DataSet/Code')
list.files()
orig_input_no_Y=read.csv('vif_sel_1.csv')
input=read.csv('vif_sel_12.csv')

vif_selected=function(df, threshold=20){
  max_vif=500
  while (max_vif > threshold){
    vifs = vif(df)
    max_vif=max(vifs)
    print(vifs[vifs==max(vifs)])
    df=df[,names(df)[!names(df) %in% names(vifs[vifs==max(vifs)])]]
    
  }
  return(df)
}

####################################################################################
#The analysis below is for all features. One below that will omit Zernicke moments.
####################################################################################

res=vif_selected(input)
head(res)
dim(res) #4556  114

res.10=vif_selected(res, 10) #4556  101
dim(res.10)
 
res.all=vif_selected(orig_input_no_Y,10) #4556  101
dim(res.all) #4556  101
