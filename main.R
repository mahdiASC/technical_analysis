library(dplyr)
library(Hmisc)
library(stringr)

##Loading Data
rawData<-read.csv('raw.csv', stringsAsFactors = FALSE)
endData<-read.csv('ending.csv', header = TRUE,stringsAsFactors = FALSE)

#INCLUDE TECHNICAL DATA
preData<-read.csv('pre_tech.csv',stringsAsFactors = FALSE, header=TRUE)
postData<-read.csv('post_tech.csv',stringsAsFactors = FALSE, header=TRUE)

#double spaces are an issue
fixer<-function(string){
  return(str_replace(gsub("\\s+", " ", str_trim(tolower(string))), "B", 'b'))
}

#cleaning data
endData['Name']<-apply(endData,1,function(x){
  return(fixer(paste(x['First.Name'], x['Last.Name'], sep=' ')))
})

rawData['Part.1']<-apply(rawData['Part.1'],1,function(x){
  return(fixer(x)) 
})

#Selecting names
selectionName<-rawData[rawData[["Part.1"]] %in% endData[['Name']],]

selectionEmail.all<-rawData[rawData[["Personal.Email"]] %in% endData[['Student.Email']],]
selectionEmail<-selectionEmail.all[selectionEmail.all[["Part.1"]] %nin% endData[['Name']],]

selectionEmail[['Part.1']]<-apply(selectionEmail,1,function(x){
  index<-which(endData[['Student.Email']]==x['Personal.Email'])
  return(endData[index,'Name'])
})

#149 total
total_selection<-rbind(selectionName,selectionEmail)

#removing dups
dup_names<-names(which(table(total_selection['Part.1'])!=1))
dup_indx<-which(total_selection[['Part.1']]==dup_names)

#viewing
#total_selection[dup_indx,]

total_selection<-total_selection[-c(142,143),]

#removing unneeded columns and changing column names
total_selection<-total_selection[,c(2,24,28,29,38,40)]
#total_selection<-total_selection[,c(2,24,28,29,34,38,40)] #cohorts

#cleaning technical data
preData<-preData[,-c(1,2,16)]
postData<-postData[,-c(1,2,16)]
headerNames<-c('Name','Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12')
colnames(preData)<-headerNames
colnames(postData)<-headerNames

#standardizing names
preData['Name']<-apply(preData['Name'],1,function(x){
  return(fixer(x)) 
})

postData['Name']<-apply(postData['Name'],1,function(x){
  return(fixer(x)) 
})

#Pre
#removing dup names
dup_names<-names(which(table(preData['Name'])!=1))
dup_indx<-which(preData[['Name']]%in%dup_names)

#viewing
preData[dup_indx,]
#user last entries
preData <- preData[-c(24,44,60),]

##Post
#removing dup names
dup_names<-names(which(table(postData['Name'])!=1))
dup_indx<-which(postData[['Name']]%in%dup_names)
#NONE

##filtering only names found on both lists
goodNames<-intersect(postData[['Name']], preData[['Name']])
  
badNames<-setdiff(postData[['Name']], preData[['Name']])

##filtering only names found on all lists
bestNames <- intersect(goodNames, total_selection[['Part.1']])

worstNames<- setdiff(goodNames, total_selection[['Part.1']])

#converting answers to boolean
convert_answers <- function(y){
  x<-logical()
  
  #x is a row from data sheet
  x<-c(x,y['Q1']=='pwd')
  x<-c(x,y['Q2']=='mkdir {directory}')
  x<-c(x,y['Q3']=='cd {directory}')
  x<-c(x,y['Q4']=='touch {file}') 
  x<-c(x,y['Q5']=='ls')
  x<-c(x,y['Q6']=='Option 1')
  x<-c(x,y['Q7']=='"Purple Lurple"')
  x<-c(x,y['Q8']=="There's a missing closing bracket, The \"testing\" function is not properly defined, The function should surround the argument with parenthesis, instead of square brackets")
  x<-c(x,y['Q9']=="The object should use colons instead of equal signs, The object should open and close with curly brackets, The key/value pairs should be separated by commas")
  x<-c(x,y['Q10']=="sally.talk() will console log \"Hello, my name is Sally\", bob.name is \"Bob\"")
  x<-c(x,grepl(y['Q11'],"The second div is not properly closed")&&grepl(y['Q11'],"The third div is missing an opening p tag"))
  x<-c(x,y['Q12']=='p {font-weight:bold;}')
  x<-as.logical(x)
  return(t(rbind(y['Name'],data.frame(x))))
}

preData.prelim<-t(apply(preData,1,convert_answers))
postData.prelim<-t(apply(postData,1,convert_answers))

colnames(preData.prelim)<-headerNames
colnames(postData.prelim)<-headerNames

#combining total_selection and total_technical
finalData<-merge(merge(preData.prelim,postData.prelim, by='Name'), total_selection, by.x='Name', by.y='Part.1')

#START OF ANALYSIS

#Exploratory analysis

techRaw<-apply(finalData[,c(2:25)], 2,function(x){
  round(mean(as.logical(x)),digits=3)
})

techRaw.comp = numeric()
for(x in 1:(length(techRaw)/2)){
  strx = paste("Q",x,".x", sep='')
  stry = paste("Q",x,".y", sep='')
  #browser()
  techRaw.comp <- c(techRaw.comp, techRaw[stry]-techRaw[strx])
}

mean(techRaw.comp)      
#improvement is ~40% across all technical fields

#omitting ambiguous results
mean(techRaw.comp[-c(8:10)])
#improvement is ~47% across all technical fields

#adding each student's final tech score as well as difference in score
finalData['techScore.pre']<-apply(finalData,1,function(x){
  #browser()
  mean(as.logical(x[2:13]))
})

finalData['techScore.post']<-apply(finalData,1,function(x){
  #browser()
  mean(as.logical(x[14:25]))
})

finalData['techScore.diff']<-apply(finalData,1,function(x){
  #browser()
  as.numeric(x['techScore.post'])-as.numeric(x['techScore.pre'])
})

##Data for Lexie##

#data.lex<-subset(finalData,select=c(Name,Final.Score, Logic.Score, techScore.pre, techScore.post, techScore.diff))
#names<-c("Jesus Castano","Seth Roberts","Hasaan Ismaeli","Donald  Poindexter ","Jerome McCree","Isaiah Massey","Marcus Stevens","Ahmed Anthony","Marcus Jones","Isaiah Kearney","Jordan  Jiles","Eric Humphries","Brady Walker","Jeremiah Alford","Jayden  Walker","Ke'Von Brown","Darius  Watts","mouhamed  barry")
#names<-sapply(names,fixer)
#data.lex.all<-data.lex[data.lex[["Name"]] %in% names,] #no rows?
#summary(data.lex.all)

##End of data for lexie##

#Building out statistics options for quantitative and qualitative data
pcrData<-subset(finalData,select=c(Final.Score, Logic.Score,Grade, School.Type, Previous.CS.Experience, techScore.pre, techScore.post, techScore.diff))
#pcrData<-subset(finalData,select=c(Name, Final.Score, Logic.Score,Grade, School.Type, Cohort, Previous.CS.Experience, techScore.pre, techScore.post, techScore.diff))

#write.csv(pcrData,'cohorts.csv', row.names=FALSE)

###SENDING TO HEZEL
#hzl.data<-subset(finalData,select=c(Name,Final.Score, Logic.Score,Grade, School.Type, Previous.CS.Experience, techScore.pre, techScore.post, techScore.diff))
#colnames(hzl.data)<-c("name",'read_score','logic_score','grade','school_type','cs_exp','tech_score.pre','tech_score.post','tech_score.change')
#write.csv(hzl.data,'tech_data.csv', row.names=FALSE)

#correcting Data types
pcrData <- transform(pcrData, Grade = as.factor(Grade),School.Type = as.factor(School.Type),Previous.CS.Experience = as.factor(Previous.CS.Experience))

pcrData.num <- pcrData[,c(1,2,6:8)]
pcrData.qual <- pcrData[,c(3:8)]
#Selecting variables for model (NUMERIC)

prin_comp<-prcomp(pcrData.num, scale = T)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2
round(pr_var/sum(pr_var),digits=3)
#first 3 components most significant
#PC1 - (Pre-SI vs. Post-SI) clearly delineates pre and post SI
#PC2 - (strong foundation for learning rooted in logical thinking vs. reading and tech.pre) all factors correlate, but logic score and tech.post correlate strongest
#PC3 - (reading and writing vs. logic and tech) shows tech.pre and tech.post are distinct from reading and logic scores

#run a decision tree
#trees need categorical outcomes
for(k in 6:8){
  newSummary <- summary(pcrData[,k])
  name <- paste(names(pcrData)[k],".cat", sep='')
  pcrData[name] <- sapply(pcrData[,k],function(x){
    if(x>newSummary['3rd Qu.']){
      return('Top')
    }else if(x>newSummary['Median']){
      return('Above Avg')
    }else if(x>newSummary['1st Qu.']){
      return('Below Avg')
    }else{
      return('Low')
    }
  })
}

#Making training and testing data
set.seed(123)
train = sample(1:nrow(pcrData), .8*nrow(pcrData)) #80% going into training
test = -train
trainData = pcrData[train,]
testData = pcrData[test,]
library(rpart)
library(rpart.plot)
library(party)

#post focus
rpart.model.post<-rpart(techScore.post.cat~Grade + School.Type + Previous.CS.Experience + Logic.Score + Final.Score,data=trainData, control=rpart.control(cp=0.05))
#plotting
rpart.plot(rpart.model.post)

#diff focus
rpart.model.diff<-rpart(techScore.diff.cat~Grade + School.Type + Previous.CS.Experience + Logic.Score + Final.Score,data=trainData)

#plotting
#plot(rpart.model,asp=28,margin=0.05)
#text(rpart.model, pretty=0)
rpart.plot(rpart.model.diff)

#Conclusion: Post focus is not something we can build model for!

#Selecting variables for model (QUALITATIVE)
#rpart.model <- rpart(techScore.diff ~ Grade+School.Type+Previous.CS.Experience, data = pcrData, method = "anova")

#Create various predictive models
#test accuracy of models
rpart.prediction <- predict(rpart.model.diff, testData, type = 'class')

#misclassifcation error
mean(rpart.prediction!=testData$techScore.diff.cat)

###NEW PREDICTION MODELS
#outcome is continous
#with dichotomous explanatory variables

#Using linear Regression for continous variables
#Using ANOVA for categorical variables

#separating the variables

lm.train<-trainData[,-c(3:5,9:11)]
lm.test<-testData[,-c(3:5,9:11)]
anova.train<-trainData[,-c(1,2, 6:8)]
anova.test<-testData[,-c(1,2, 6:8)]

#linear regression
library(caret)

#https://www.r-bloggers.com/r-tutorial-series-multiple-linear-regression/

#diff is NOT a good way to predict
lm.diff <- lm(techScore.diff~Final.Score+Logic.Score, data=lm.train)
summary(lm.diff)

#post is way more effective at predicting!
lm.post <- lm(techScore.post~Logic.Score+techScore.pre, data=lm.train)
summary(lm.post)

#cross validation

library(DAAG)

#1 continuous variable at a time
#Nothing signficant about techScore.diff
cv.lm(lm.train, techScore.post~techScore.pre) #p<.056
cv.lm(pcrData.num, techScore.post~Logic.Score) # .014


#Assessing Categorical data
# General Linear Model
glm()
