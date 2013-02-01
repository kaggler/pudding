rpart.run=function(formula,cplist,trainset,testset)
{
	if (is.factor(trainset[,1]))
		trainset[,1]=as.numeric(trainset[,1])-1
	pred=rep(0,nrow(testset))
	for (cp in cplist)
	{
		model=rpart(formula,data=trainset,method="anova",cp=cp)
		pred=pred+predict(model,newdata=testset)
		show(cp)
	}
	return(pred)
}

logit.run=function(formula,varlist,trainset,testset)
{
	pred=rep(0,nrow(testset))
	for (i in 1:ncol(varlist))
	{
		varind=which(varlist[,i]==1)
		model=glm(formula,data=trainset[,c(1,varind)],family=binomial)
		pred=pred+predict(model,newdata=testset[,varind])
		show(i)
	}
	return(pred)
}

ridge.run=function(formula,varlist,trainset,testset)
{
  pred=rep(0,nrow(testset))
  for (i in 1:ncol(varlist))
  {
    varind=which(varlist[,i]==1)
    model=lm.ridge(formula,data=trainset[,c(1,varind)])
    pred=pred+apply(testset[,varind],2,as.numeric)%*%as.matrix(coef(model)[-1])+coef(model)[1]
    show(i)
  }
  return(pred)
}

cplist=c(1,1,2,3,4,5,6,7,10,11,12,13)/1000
rpart.pred=rpart.run(worse~.,cplist=cplist,trainset=train,testset=test)

varlist=logit.var[,c(14,15,17,26,40,44,50)]
logit.pred=logit.run(worse~.,varlist=varlist,trainset=train,testset=test)

rvarlist=ridge.var[,c(20,38)]
train1=train; train1$worse=as.numeric(train1$worse)-1
ridge.pred=ridge.run(worse~.,varlist=rvarlist,trainset=train1,testset=test)




###output###
rpart.logit.pred=(rpart.pred+logit.pred)/17
rpart.logit.pred=(rpart.logit.pred-min(rpart.logit.pred))/(max(rpart.logit.pred)-min(rpart.logit.pred))

id=1:nrow(test)

ans=data.frame(id=id,pred=rpart.logit.pred)
write.csv(ans,file="RpartLogitEnsemble.csv",row.names=F,col.names=F)

pred1=ridge.pred/2
f.pred=data.frame(id=id,pred=(pred1-min(pred1))/(max(pred1)-min(pred1)))
write.csv(f.pred,file="r.pred.csv",row.names=F,col.names=F)