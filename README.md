# Pokemon-Classification-

##   Data   Wrangling

#   Load   in   the   data Pokemon=read.csv(file="Pokemon.csv",head=TRUE,na.strings=c( ""))
library(caret)
#   Get   the   general   information   of   the   dataframe str(Pokemon)
attach(Pokemon)


##   EDA

#   Missing   Value   Check
#   Check   for   the   missing   values   and      look   how   many   unique   values   there   are   for   each   variable   using   the sapply()
dim(Pokemon)
sapply(Pokemon,function(x)   sum(is.na(x)))
#   Summarize   the   unique   values
sapply(Pokemon,   function(x)   length(unique(x)))
#   Make   a   visual   take   on   the   missing   values
library(Amelia)
missmap(Pokemon,   main   =   "Missing   values   vs   observed")
#   Deal   with   the   missing   value
Pokemon$Type.1[is.na(Pokemon$Type.1)]="Poison"
#   Color   the   different   labels   using   plots
Pokemon$Generation=factor(Pokemon$Generation) Pokemon$Legendary=factor(Pokemon$Legendary)
Pokemon$Type.2=as.character(Pokemon$Type.2)


##   Data   Visualization
sapply(c("ggplot2","gridExtra","ggExtra","dplyr"),   library,   character.only   =   T,   logical.return   =   T,   quietly =   T,   warn.conflicts   =   F)
colors   <-   c("#593420","#080c26","#7d8228", "#e2dc2f","#fcdefb","#662714","#c91f01","#d0edef","#7f7682","#2d6633","#5b5550","#8dd9f4","#e0e0 e0","#126347","#4a116b","#332f28","#bababa","#203bc1")
##   Check   the   distribution   of   Type.1   Pokemon ggplot(Pokemon,aes(x=Type.1,fill=Type.1))+
       geom_histogram(stat="count",color="black")+
      scale_fill_manual(values=as.character(colors))+
      theme(axis.text.x   =   element_text(angle   =   90,   hjust   =   1))+labs(title="Number   of   Pokemon   per Type.1",x="Type.1",y="Count")+theme_light()


#   check   the   distribution   of   Type.2   Pokemon
ggplot(Pokemon,aes(x=Type.2,fill=Type.2))+
      geom_histogram(stat="count",color="black")+
      scale_fill_manual(values=as.character(colors))+
      theme(axis.text.x   =   element_text(angle   =   90,   hjust   =   1))+labs(title="Number   of   Pokemon   per Type.2",x="Type.2",y="Count")+theme_light()

# Conclusion   drawn   from   the   EDA:
# Nearly   half   of   the   Pokemon   have   only   one   primary   type. Flying   type   as   the   first   type   is   very   rare.
# The   water   and   normal   are   the   most   common   primary   type.


##   Question   1:Classification   for   Types
###   Question1.1   Classification   for   only   one   type
#   Fit   a   random   forest   model   to   ask   question   1
#   Using   Caret   Method
#   A   crucial   step   to   make   caret   library   work
Pokemon$Legendary   <-   factor(Pokemon$Legendary,   labels   =   c("yes",   "no")) Legendary=factor(Pokemon$Legendary)
#   Set   a   fitcontrol   model
fitControl   <-   trainControl(method   =   "cv",number   =   10)
#   make   sure   no   empty   classes   in   y
Pokemon$Type.1[is.na(Pokemon$Type.1)]="Poison" RF1=train(Pokemon[,-(1:4)],Pokemon[,3],method="rf",trControl   =   fitControl,verbose   =   FALSE) print(RF1)
varImp(RF1)
plot(RF1)
#   boosted   tree   method
gbmFit1=train(Pokemon[,-(1:4)],Pokemon[,3],method="gbm",trControl   =   fitControl,verbose   =   FALSE) print(gbmFit1)
varImp(gbmFit1)
plot(gbmFit1)


##   Check   the   combination   of   Types Collinearity   Check

library(GGally)

ggpairs(Pokemon[,6:11]) 

#   copy   a   dataframe
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(caret) pokemon.copy=read.csv(file="E:/Xu/Cornell/STSCI4740_SL/Project/Pokemoncopy.csv",head=TRUE,na .strings=c(""))
#   add   a   new   columnn
library(tree) form=as.formula(pokemon.copy$DualType~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Gener ation)
tree.1   <-   rpart(form,data=pokemon.copy,control=rpart.control(minsplit=20,cp=0))
prp(tree.1,varlen=15)


##  Question1.2   Classification   for   two   types ####   Dual   Type   Analaysis   Function
##   Question2:BUilding   Predicting   Models
#   Fit   a   logistic   regression   model   on   the   Legendary   Variable   to   answer   the   question   2
logmodel   <- glm(Legendary~Defense+Sp..Atk+Sp..Def+Speed+Generation,family=binomial,data=Pokemon) summary(logmodel)
#   Interpret   the   results   of   logistic   regression   model
#   First   of   all   the   Attack   variable   is   not   statistically   significant
#   All   the   coefficients   are   positive,which   means   the   higher   the   attributes,   the   more   likely   a   poker   man   is going   to   be   a   legendary
anova(logmodel,test="Chisq")
library(boot)
cost   <-   function(r,pi   =0)   mean(abs(r-pi)   >   0.5)
cv.err=cv.glm(Pokemon,logmodel,cost,K=10)
cv.err$delta
##   Use   Caret   Library   to   Try   Different   Models library(caret)
#   A   crucial   step   to   make   caret   library   work
#   Set   a   fitcontrol   model
fitControlknn   <-   trainControl(method   =   "cv",number   =   10) 


#   Try   KNN   models   to   classify   legendary
Pokemon$Type.2[is.na(Pokemon$Type.2)]="no"
knnFit   <- train(Legendary~Type.1+Type.2+Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation, data=Pokemon,method   =   "knn",   trControl   =   fitControlknn,   preProcess   =   c("center","scale"),   tuneLength   = 20)
knnFit
plot(knnFit)


#   Try   QDA
fitControl   <-   trainControl(method   =   "cv",number   =   10)
QDAFit   <-   train(Legendary~HP+Attack+Defense+Sp..Atk+Sp..Def+Speed,   data=Pokemon, method='qda',trControl=fitControl)
QDAFit
