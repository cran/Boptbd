summary.Boptbd <-
function(object,...)
{
optbd_maes<-object
optbd_maes$grphlt<-make_graph(as.numeric(as.factor(object$OptdesF)), directed = FALSE)
class(optbd_maes)<-"summary.Boptbd"
optbd_maes
}
