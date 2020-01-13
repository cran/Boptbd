intcbd <-
function(trt.N,blk.N){
  arrays<-t(combn(trt.N,2)) 
  na=dim(arrays)[1]
  Con.egv.check=0.00000001
  while(Con.egv.check<0.000001){
    All.trt.check=trt.N-1
    while(All.trt.check <trt.N){
      des.p<-if(blk.N>na) {c(sample(1:na,na,replace=FALSE), sample(1:na,blk.N-na,replace=TRUE))} else {
        sample(1:na,blk.N,replace=FALSE)}
      des<-t(arrays[des.p,])
      trtin<-contrasts(as.factor(des),contrasts=FALSE)[as.factor(des),]
      R.trt<-t(trtin)%*%trtin
      All.trt.check<-rankMatrix(R.trt)
    }
    cmato=cmatbd(trt.N,blk.N, 0,des)
    egv<-sort(eigen(cmato)$values)
    Con.egv.check<-egv[2]
  }
  return(des)
}
