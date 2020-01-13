graphoptBbd <-
function(trt.N, blk.N,alpha,beta,OptdesF,Optcrit) {
  trtblkthetano<-paste("(",paste(trt.N, blk.N, alpha, beta, sep=", "),")",sep="")
  
  NOptcrtr<-paste(Optcrit,"-optimal",sep="")#name of optimality criteria
  NOptcrtrG<-paste("Graph_layout_B",Optcrit,"optbd",sep="")#name of folder where the graphical layout will be saved
  NOptcrtrG2<-paste("_Gout_B",Optcrit,"optbd.pdf",sep="")
  NgoutT=paste("Graphical layout of Bayesian",NOptcrtr, "block", "design", sep=" ")
  NgoutT1=paste(paste("(v, b, alpha, beta) =",trtblkthetano,sep=" "))
  graph.des <- make_graph(as.numeric(as.factor(OptdesF)), directed = FALSE)
  graph.desid <- tkplot(graph.des, canvas.width=515, canvas.height=500,layout=layout.kamada.kawai,vertex.color="cyan",edge.color="black")
  canvas <- tk_canvas(graph.desid)
  padding <- 100
  coords <- norm_coords(layout=layout.kamada.kawai(graph.des), 0+padding, 450-padding,
                        50+padding, 500-padding)
  tk_set_coords(graph.desid, coords)
  width <- as.numeric(tkcget(canvas, "-width"))
  height <- as.numeric(tkcget(canvas, "-height"))
  tkcreate(canvas, "text", width/2, 25, text=NgoutT,
           justify="center", font=tcltk::tkfont.create(family="helvetica",size=12,weight="bold"))
  tkcreate(canvas, "text", width/2, 45, text=NgoutT1,
           justify="center", font=tcltk::tkfont.create(family="helvetica",size=12,weight="bold"))
  graph.OutlayoptBlk<-tempdir()
  obtdes.goutloptBlk<-paste(graph.OutlayoptBlk,paste(trtblkthetano,NOptcrtrG2,sep=""),sep="/")
  obtdes.goutloptBlk2<-paste(graph.OutlayoptBlk,trtblkthetano,sep="\\")
  pdf(file=obtdes.goutloptBlk)
  plot(graph.des,edge.arrow.size=5, vertex.size=20, margin=0.5,
       layout=layout.kamada.kawai,vertex.color="cyan",edge.color="black")
title(paste("Graphical layout of Bayesian ", Optcrit,"-optimal block design",sep=""), 
      sub = paste("Bayesian ",Optcrit,"-optimal block design",sep=""),cex.main = 1,   font.main= 1, col.main= "black")
mtext(paste("(v, b, alpha, beta) = (",paste(trt.N, blk.N, alpha, beta, sep=", "),")",sep=" "), line = -0.50, col = "blue", font = 1)
  dev.off()  
  file_loc<-graph.OutlayoptBlk
  file_loc2<-paste("Graphical layout of Bayesian ", NOptcrtr, " block design is also saved in .pdf at:",sep=" ")
  message(file_loc2,"\n",file_loc,"\n\n")
}
