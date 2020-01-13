fixparBbd <-
function(Optcrit){
  trt.I<-tclVar(paste("3"));#Number of treatments (default)
  blk.I<-tclVar(paste("3"));#Number of blocks (default)
  alpha.I<-tclVar(paste("0.1"));#Alpha value (default)
  beta.I<-tclVar(paste("0.1"));#Beta value (default)
  brep.I<-tclVar(paste("10"));#Nuber of MCMC selection (default)
  rep.I<-tclVar(paste("10"));#Number of replications (default)
  itrcvrgval.I<-tclVar(paste("6"));#Initial convergence value (default)
  cbValue.I<-tclVar("0")
  cbValue3.I<-tclVar("0")  
#"=============================================================================="
optcrtF<-function(Optcrit){
   nrep<-as.numeric(tclvalue(rep.I))
   trt.N<-as.numeric(tclvalue(trt.I))
   blk.N<-as.numeric(tclvalue(blk.I))
   alpha<-as.numeric(tclvalue(alpha.I))
   beta<-as.numeric(tclvalue(beta.I))
   brep<-as.numeric(tclvalue(brep.I))
   cbVal<-as.numeric(tclvalue(cbValue.I))
   itr.cvrgval<-as.numeric(tclvalue(itrcvrgval.I))
   if(itr.cvrgval>blk.N) itr.cvrgval<-blk.N
  cbVal3<-as.numeric(tclvalue(cbValue3.I))
if(Optcrit=="A"){
   optbdFS=Boptbd(trt.N,blk.N,alpha,beta,nrep,brep,itr.cvrgval,Optcrit="A")} 
if(Optcrit=="D")  {
   optbdFS=Boptbd(trt.N,blk.N,alpha,beta,nrep,brep,itr.cvrgval,Optcrit="D")}
if(cbVal3=="1") optbdFS=summary(optbdFS)
  print(optbdFS)
if(cbVal=="1") {graphoptBbd(trt.N, blk.N,alpha,beta,optbdFS$OptdesF,Optcrit)}
}
#"=============================================================================="

  fiPar<-tktoplevel()
  fontHeading<- tkfont.create(family="times",size=40,weight="bold")
  fontHeading3<-tkfont.create(family="times",size=10,weight="bold")
  fontHeading2<-tkfont.create(family="times",size=14,weight="bold")
  fontHeading4<-tkfont.create(family="times",size=12,weight="bold")
  tkwm.title(fiPar,"Set parameter values")
  fiParF<-tkframe(fiPar)
  fiParFup<- tkframe(fiParF,relief="groove",borderwidth=2)
  fiParFmid<-tkframe(fiParF,relief="sunken",borderwidth=2)
  fiParFlow<-tkframe(fiParF,relief="sunken",borderwidth=2)
  fiParFlow2<-tkframe(fiParF,relief="groove",borderwidth=2)
  trt.in<-trt.I
  blk.in<-blk.I
  alpha.in<-alpha.I
  beta.in<-beta.I
  brep.in<-brep.I
  rep.in<-rep.I
  itrcvrgval.in<-itrcvrgval.I
  cbValue.in<-cbValue.I
  cbValue3.in<-cbValue3.I
  trt.i1<-tkentry(fiParFup,width=11,textvariable=trt.in)
  blk.i1<-tkentry(fiParFup,width=11,textvariable=blk.in)
  alpha.i1<-tkentry(fiParFup,width=11,textvariable=alpha.in)
  beta.i1<-tkentry(fiParFup,width=11,textvariable=beta.in)
  brep.i1<-tkentry(fiParFup,width=11,textvariable=brep.in)
  rep.i1<-tkentry(fiParFup,width=11,textvariable=rep.in)
  itrcvrgval.i1<-tkentry(fiParFup,width=11,textvariable=itrcvrgval.in)
  cbValue.i1<-tkcheckbutton(fiPar)
  tkconfigure(cbValue.i1,variable=cbValue.in)
  cbValue.i3<-tkcheckbutton(fiPar)
  tkconfigure(cbValue.i3,variable=cbValue3.in)
  tkgrid(tklabel(fiParF,text="       Fix Value of Parameters     ",font=fontHeading2))
  tkgrid(tklabel(fiParFup,text="Number of treatments:                   "),trt.i1)
  tkgrid(tklabel(fiParFup,text="Number of blocks (arrays):             "),blk.i1)
  tkgrid(tklabel(fiParFup,text="Alpha value:                                      "),alpha.i1)
  tkgrid(tklabel(fiParFup,text="Beta value:                                         "),beta.i1)
  tkgrid(tklabel(fiParFup,text="Number of MC selections:              "),brep.i1)
  tkgrid(tklabel(fiParFup,text="Number of replications:                  "),rep.i1)
  tkgrid(tklabel(fiParFup,text="Iterations for exchange Procedure: "),itrcvrgval.i1)
  tkgrid(tklabel(fiParFmid,text="Show graphical layout        ", font=fontHeading4),cbValue.i1)
  tkgrid(tklabel(fiParFmid,text="Show Summary result        ", font=fontHeading4),cbValue.i3)
  tkgrid(tklabel(fiParFlow,text=" ",font=0.01))#empty line
  exitFP<-function() {
    closeQ=tkmessageBox(title = "Exit set parameter values", message = "You are leaving set parameter values window",
                        icon = "info", type = "okcancel", default = "cancel")
    if(as.character(closeQ)=="ok") tkdestroy(fiPar)
  }#end of exitFP
  exit.but2a<-tkbutton(fiParF,text="Exit",command=exitFP,width=10)
  serch.but<-function(Optcrit){
    if (Optcrit=="A") but.A<-tkgrid(tkbutton(fiParFlow2,text="Search Bayesian A-opt block design",command=function()optcrtF("A"),width=27), exit.but2a)
    if (Optcrit=="D") but.A<-tkgrid(tkbutton(fiParFlow2,text="Search D-opt block design",command=function()optcrtF("D"),width=22), exit.but2a)
    return(but.A)
  }#end of serch.but
  tkgrid(tklabel(fiParFlow2,text="                                                    ",font=0.00001))
  serch.but(Optcrit)
  tkgrid(tklabel(fiParFlow2,text="                                             ",font=0.00001))
  tkgrid(fiParFup)
  tkgrid(tklabel(fiParF,text="---------------------------------", font=fontHeading3))
  tkgrid(fiParFlow)
  tkgrid(tklabel(fiParF,text="---------------------------------", font=fontHeading3))
  tkgrid(fiParFmid)
  tkgrid(tklabel(fiParF,text="---------------------------------", font=fontHeading3))
  tkgrid(fiParFlow2)
  tkgrid(fiParF) 
}
