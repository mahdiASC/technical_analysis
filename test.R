##Loading Data
startData<-read.csv('incoming.csv', stringsAsFactors = FALSE)
endData<-read.csv('ending.csv', header = TRUE,stringsAsFactors = FALSE)


##Cleaning Data
endData['Name']<-apply(endData,1,function(x){
  return(paste(x['First.Name'], x['Last.Name'], sep=' '))
})

endData[,-c(1,2)]

#checking/eliminating repeats
which(table(endData['Name'])!=1)
which(endData['Name']=='Andreas Ramdas')
#second is most accurate

endData<-endData[-85,]

which(table(startData['Name'])!=1)

blank_start_email<-startData["ï..Primary.Email"][which(startData['Name']==""),]
startNames <- names(which(table(startData['Name'])!=1))

for(i in startNames){
  cat(i)
  cat(which(startData['Name']==i))
}

#Selecting most accurate (not eliminating blanks, will check emails first)
startData<-startData[-c(104,51,62,106,58),]

#No emails found?
for(k in blank_start_email){
  apply(endData,1,function(x){
    print(x['Student.Email'])
    if(x['Student.Email']==k){
      print(x['Name'])
    }
  })
}

#eliminating blanks from start
startData<-startData[-which(startData['Name']==''),]
