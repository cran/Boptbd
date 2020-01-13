mmenuBbd <-
function(){
  fontHeading<- tkfont.create(family="times",size=40,weight="bold")#,slant="italic")
  fontHeading3<-tkfont.create(family="times",size=10,weight="bold")
  AMVDEopt.top<-tktoplevel()
  tkwm.title(AMVDEopt.top,"Bayesian Optimal Block Designs")
  tkgrid(tklabel(AMVDEopt.top,text="     Bayesian_optbd 1.0.5     ",font=fontHeading))
  tkgrid(tklabel(AMVDEopt.top,text="",font=fontHeading))
  Fixp.butA<-tkbutton(AMVDEopt.top,text="Bayesian A-Optimal Block Design",font=fontHeading3,command=function()fixparBbd("A"),width=30)
  Fixp.butD<-tkbutton(AMVDEopt.top,text="Bayesian D-Optimal Block Design",font=fontHeading3,command=function() fixparBbd("D"),width=30)
  exitMM<-function() {
    closeQ=tkmessageBox(title = "Bye...", message = "Bye..., Enjoy your optimal design",
                        icon = "info", type = "okcancel", default = "cancel")
    if(as.character(closeQ)=="ok") tkdestroy(AMVDEopt.top)
  }#end of exitMM
  ExitWin.but<-tkbutton(AMVDEopt.top,text="Exit",font=fontHeading3,command=exitMM,width=15)
  tkgrid(Fixp.butA)
  tkgrid(tklabel(AMVDEopt.top,text="",font=fontHeading3))
  tkgrid(Fixp.butD)
  tkgrid(tklabel(AMVDEopt.top,text="",font=fontHeading3))
  
  tkgrid(ExitWin.but)
  
  tkgrid(tklabel(AMVDEopt.top,text="",font=fontHeading))   }
