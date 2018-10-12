alldata=read.csv('Ames_data.csv',stringsAsFactors = F)
aaa=read.table('Project1_test_id.txt', header = FALSE, sep = "", dec = ".")

print(sum)
alldata=read.csv('Ames_data.csv',stringsAsFactors = F)
test.id = aaa[,10]
train=alldata[!alldata$PID %in% test.id,]
test=alldata[alldata$PID %in% test.id,]
write.csv(train, file="train.csv",row.names = F)
write.csv(test[,-dim(test)[2]], file="test.csv",row.names = F)
aaaa=data.frame(PID=test.id,True_Sale_Price=test[,dim(test)[2]])
write.csv(aaaa,file = "price.txt",sep=',',row.names = F)


