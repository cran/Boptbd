Baoptbd <-
function(trt.N,blk.N,alpha,beta,nrep,brep,itr.cvrgval) {
  #House keeping
  arrays=t(combn(trt.N,2))
  na=dim(arrays)[1]
  del.1<-matrix(1000,na,3)
  desbest.1<-matrix(0,nrep*2,blk.N)
  aoptbest.1<-matrix(0,nrep,2)
  rsampb<-setdiff(unique(round(rbeta(brep,alpha,beta),5)),c(0,1))
  nmcmc<-length(rsampb) 
  #Start iteration
  for(irep in 1:nrep){
    #Initial design with its corresponding Ascore value
    des<-intcbd(trt.N,blk.N)
    if(trt.N==blk.N&trt.N>3&irep<(trt.N-1)) {in.desns=matrix(0,(trt.N-3)*2,blk.N)
    in.desns0=rbind(seq(1,trt.N),c(seq(1,trt.N)[2:trt.N],1))
    for(i in 1:(trt.N-3)) {in.desns01=cbind(rbind(seq(1,(trt.N-i)),c(seq(1,(trt.N-i))[2:(trt.N-i)],1)),rbind(rep(1,i),((trt.N-i+1):trt.N))); in.desns[c((i-1)*2+1,i*2),]=in.desns01}
    in.desns=rbind(rbind(seq(1,trt.N),c(seq(1,trt.N)[2:trt.N],1)),in.desns)
    des=in.desns[c((irep-1)*2+1,irep*2),]}
    
    aopts={};
    imcmc=1;
    for(imcmc in 1:nmcmc){
      rv=1;
      theta=rsampb[imcmc];
      cmat<-cmatbd(trt.N,blk.N,theta,des)
      aopts1=sum(diag(ginv(cmat)))
      aopts<-rbind(aopts,aopts1)
    }
    aopt=mean(aopts)
    
    acold=aopt
    descold=t(des)
    cdel=100
    i=1;
    ivalacold={}
    for(i in 1:blk.N){
      for(j in 1:na){
        temp=descold[i,]
        if(all(descold[i,]==arrays[j,]))  {aopt=acold; del.1[j,]<-c(j,(acold-aopt),aopt); next}
        descold[i,]=arrays[j,]
        trtin<-contrasts(as.factor(t(descold)),contrasts=FALSE)[as.factor(t(descold)),]
        R.trt<-t(trtin)%*%trtin
        if (rankMatrix(R.trt)[1]<trt.N)  {aopt=1000; del.1[j,]<-c(j,(acold-aopt),aopt); next}
        cmato=cmatbd(trt.N,blk.N, 0,t(descold))
        egv<-sort(eigen(cmato)$values)
        if(egv[2]<0.000001) {aopt=1000; del.1[j,]<-c(j,(acold-aopt),aopt); next}
        
        aopts={};
        imcmc=1;
        for(imcmc in 1:nmcmc){
          rv=1;
          theta=rsampb[imcmc];
          cmat=cmatbd(trt.N,blk.N,theta,t(descold))
          aopts1=sum(diag(ginv(cmat)))
          aopts<-rbind(aopts,aopts1)
        }
        aopt=mean(aopts)
        del.n<-del.1[j,]<-c(j,(acold-aopt),aopt)
        descold[i,]=temp
      }
      del.1<-del.1[order(del.1[,3]),]
      delbest=t(del.1[1,])
      descold[i,]=arrays[delbest[1],]
      acold=delbest[3]
      #print(acold)
      cdel=delbest[2]
      ivalacold=rbind(ivalacold, c(i,acold))
      if(i>itr.cvrgval) if(all(ivalacold[c(i-(itr.cvrgval-2),i),2]==ivalacold[i-(itr.cvrgval-1),2])) break
    }
    next.it<- if (irep==1) {desbest.1=t(descold)} else {desbest.1=rbind(desbest.1,t(descold))}
    aoptbest.1[irep,]=c(irep,acold)
  }
  best=aoptbest.1[order(aoptbest.1[,2]),]
  nb=best[1,1]
  Ascore<-best[1,2]
  Aoptde<- desbest.1[c((nb-1)*2+1,nb*2),]
  if(trt.N!=3) {tkmessageBox(title="Search completed",message=paste("Search completed",sep=""))}
  cnames=paste0("blk",1:blk.N)
  dimnames(Aoptde)=list(NULL,cnames)
  Aopt_sum2<-list("v"=trt.N,"b"=blk.N,alpha=alpha,beta=beta,nrep=nrep, brep=brep,itr.cvrgval=itr.cvrgval, "OptdesF"=Aoptde,"Optcrtsv" =Ascore)
  return(Aopt_sum2)
}
