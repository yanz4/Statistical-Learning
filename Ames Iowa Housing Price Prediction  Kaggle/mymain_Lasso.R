library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

one_step_lasso = function(r, x, lam){
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) -lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}

mylasso = function(X, y, lam, n.iter = 50, standardize  = TRUE)
{
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam: lambda value
  # n.iter: number of iterations
  # standardize: if True, center and scale X and y. 
  p=dim(X)[2]
  Xmeans=rep(0, p)
  Xsd=1
  ymeans=0
  ysd=1
  # YOUR CODE
  # If standardize  = TRUE, center and scale X and Y
  if (standardize  == TRUE){
    X=scale(X)
    y=scale(y)
    Xmeans=attr(X,"scaled:center")
    Xsd=attr(X,"scaled:scale")
    ymeans=attr(y,"scaled:center")
    ysd=attr(y,"scaled:scale")
  }
  
  Xsd[colSums(is.na(X)) > 0]=1
  X[is.na(X)]=0
  # Initial values for residual and coefficient vector b
  b = rep(0, p)
  r = y
  
  for(step in 1:n.iter){
    for(j in 1:p){
      
      # YOUR CODE 
      
      # 1) Update the residual vector to be the one
      # in blue on p37 of [lec_W3_VariableSelection.pdf]. 
      r = r + X[, j] * b[j]
      
      # 2) Apply one_step_lasso to update beta_j
      b[j] = one_step_lasso(r, X[, j], lam)
      
      # 3) Update the current residual vector
      r = r - X[, j] * b[j]
    }
  }
  
  # YOUR CODE: scale back b and add intercept b0
  # For b0, check p13 of [lec_W3_VariableSelection.pdf]. 
  b=b/Xsd*ysd
  b0=ymeans-sum(b*Xmeans)
  return(c(b0, b))
}

train <- read.csv("train.csv",stringsAsFactors = F)
test <- read.csv("test.csv",stringsAsFactors = F)
trainprice=train$Sale_Price
train$Sale_Price=NULL
alldata=rbind(train,test)
alldata=alldata[,-c(1)]
trainpid=train[,1]
testpid=test[,1]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

processing=function(all){
  numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
  numericVarNames <- names(numericVars) #saving names vector for use later on
  # cat('There are', length(numericVars), 'numeric variables')
  all_numVar <- all[, numericVars]
  cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
  
  
  Charcol <- names(all[,sapply(all, is.character)])
  
  
  all$Overall_Qual<-as.integer(revalue(all$Overall_Qual, c("Very_Excellent"=10, "Excellent"=9,
                                                           "Very_Good"=8,"Good"=7,"Above_Average"=6,"Average"=5,"Below_Average"=4,
                                                           "Fair"=3, "Poor"=2   ,"Very_Poor"=1             )))
  
  
  all$Overall_Cond<-as.integer(revalue(all$Overall_Cond, c("Excellent"=9,
                                                           "Very_Good"=8,"Good"=7,"Above_Average"=6,"Average"=5,"Below_Average"=4,
                                                           "Fair"=3, "Poor"=2   ,"Very_Poor"=1             )))
  all$Lot_Shape<-as.integer(revalue(all$Lot_Shape, c("Irregular"=0,
                                                     "Moderately_Irregular"=1,"Slightly_Irregular"=2,"Regular"=3)))
  all$Exter_Qual<-as.integer(revalue(all$Exter_Qual, c("Fair"=0,"Typical"=1,"Good"=2,"Excellent"=3)))
  all$Exter_Cond<-as.integer(revalue(all$Exter_Cond, c("Poor"=0,"Fair"=1,"Typical"=2,"Good"=3,"Excellent"=4)))
  all$Bsmt_Qual<-as.integer(revalue(all$Bsmt_Qual, c("No_Basement"=0,"Poor"=1,"Fair"=2,"Typical"=3,"Good"=4,"Excellent"=5)))
  all$Bsmt_Cond<-as.integer(revalue(all$Bsmt_Cond, c("No_Basement"=0,"Poor"=1,"Fair"=2,"Typical"=3,"Good"=4,"Excellent"=5)))
  all$Bsmt_Exposure<-as.integer(revalue(all$Bsmt_Exposure, c("No_Basement"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)))
  all$BsmtFin_Type_1<-as.integer(revalue(all$BsmtFin_Type_1, c("No_Basement"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,'ALQ'=5,'GLQ'=6)))
  all$BsmtFin_Type_2<-as.integer(revalue(all$BsmtFin_Type_2, c("No_Basement"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,'ALQ'=5,'GLQ'=6)))
  all$Heating_QC<-as.integer(revalue(all$Heating_QC, c("Poor"=0,"Fair"=1,"Typical"=2,"Good"=3,"Excellent"=4)))
  all$Electrical<-as.integer(revalue(all$Electrical, c("Mix"=0,"FuseP"=1,"FuseF"=2,"FuseA"=3,"SBrkr"=4,"Unknown"=2)))
  all$Kitchen_Qual<-as.integer(revalue(all$Kitchen_Qual, c("Poor"=0,"Fair"=1,"Typical"=2,"Good"=3,"Excellent"=4)))
  all$Functional<-as.integer(revalue(all$Functional, c("Sal"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)))
  all$Fireplace_Qu<-as.integer(revalue(all$Fireplace_Qu, c("No_Fireplace"=0,"Poor"=1,"Fair"=2,"Typical"=3,"Good"=4,"Excellent"=5)))
  all$Garage_Finish<-as.integer(revalue(all$Garage_Finish, c("No_Garage"=0,"Unf"=1,"RFn"=2,"Fin"=3)))
  all$Garage_Qual<-as.integer(revalue(all$Garage_Qual, c("No_Garage"=0,"Poor"=1,"Fair"=2,"Typical"=3,"Good"=4,"Excellent"=5)))
  all$Garage_Cond<-as.integer(revalue(all$Garage_Cond, c("No_Garage"=0,"Poor"=1,"Fair"=2,"Typical"=3,"Good"=4,"Excellent"=5)))
  all$Paved_Drive<-as.integer(revalue(all$Paved_Drive, c("Dirt_Gravel"=0,"Partial_Pavement"=1,"Paved"=2)))
  all$Fence<-as.integer(revalue(all$Fence, c("No_Fence"=0,"Minimum_Wood_Wire"=1,"Good_Wood"=2,"Minimum_Privacy"=3,"Good_Privacy"=4)))
  
  all$MS_SubClass=as.factor(all$MS_SubClass)
  all$MS_Zoning=as.factor(all$MS_Zoning)
  all$Alley=as.factor(all$Alley)
  all$Street=as.factor(all$Street)
  all$Land_Contour=as.factor(all$Land_Contour)
  all$Lot_Config=as.factor(all$Lot_Config)
  all$Neighborhood=as.factor(all$Neighborhood)
  all$Condition_1=as.factor(all$Condition_1)
  all$Condition_2=as.factor(all$Condition_2)
  all$Bldg_Type=as.factor(all$Bldg_Type)
  all$House_Style=as.factor(all$House_Style)
  all$Roof_Style=as.factor(all$Roof_Style)
  all$Roof_Matl=as.factor(all$Roof_Matl)
  all$Exterior_1st=as.factor(all$Exterior_1st)
  all$Exterior_2nd=as.factor(all$Exterior_2nd)
  all$Mas_Vnr_Type=as.factor(all$Mas_Vnr_Type)
  all$Foundation=as.factor(all$Foundation)
  all$Heating=as.factor(all$Heating)
  all$Central_Air=as.factor(all$Central_Air)
  all$Garage_Type=as.factor(all$Garage_Type)
  all$Misc_Feature=as.factor(all$Misc_Feature)
  all$Sale_Type=as.factor(all$Sale_Type)
  all$Sale_Condition=as.factor(all$Sale_Condition)
  all$Mo_Sold=as.factor(as.factor(all$Mo_Sold))
  all$Year_Sold=as.factor(as.factor(all$Year_Sold))
  
  
  factornames=c(
    'MS_SubClass',
    'MS_Zoning',
    'Alley',
    'Street',
    'Land_Contour',
    'Lot_Config',
    'Neighborhood',
    'Condition_1',
    'Condition_2',
    'Bldg_Type',
    'House_Style',
    'Roof_Style',
    'Roof_Matl',
    'Exterior_1st',
    'Exterior_2nd',
    'Mas_Vnr_Type',
    'Foundation',
    'Heating',
    'Central_Air',
    'Garage_Type',
    'Misc_Feature',
    'Sale_Type',
    'Sale_Condition',
    'Mo_Sold',
    'Year_Sold'
  )
  
  Ordinalnames=c(
    'Overall_Qual','Overall_Cond','Lot_Shape','Exter_Qual','Exter_Cond','Bsmt_Qual','Bsmt_Cond',
    'Bsmt_Exposure','BsmtFin_Type_1','BsmtFin_Type_2','Heating_QC','Electrical','Kitchen_Qual',
    'Functional','Fireplace_Qu','Garage_Qual','Garage_Cond','Paved_Drive','Fence'
  )
  
  
  
  DFfactors <- all[, factornames]
  Ordinals= all[, Ordinalnames]
  DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
  combined <- cbind(DFdummies, Ordinals,all_numVar)
  
  for(j in 1:dim(combined)[2]){
    combined[,j][is.na(combined[,j])]= Mode(combined[,j][!is.na(combined[,j])])
  }
  return(combined)
  
}

combined=processing(alldata)
train=combined[1:dim(train)[1],]
test=combined[-(1:dim(train)[1]),]
trainprice=log(trainprice)
train=cbind(train,trainprice)


  

cv.out = mylasso(data.matrix(combined[1:dim(train)[1],]), data.matrix(trainprice), lam=40,standardize  = T )
pred=data.matrix(test)%*%as.matrix(cv.out[-1])+cv.out[1]
aaaa=data.frame(PID=testpid,Sale_Price=exp(pred))
colnames(aaaa)[2]='Sale_Price'
write.csv(aaaa,file = "mysubmission3.txt",row.names = F)

