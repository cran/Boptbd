print.Boptbd <-
function(x,...)
{
cat("\n        ---------------------------------------    \n")
cat("Title: Bayesian  ",x$Optcrit,"-optimal block design        ","Date: ", format(Sys.time(), "%a %b %d %Y %H:%M:%S"),"\n",sep="")
cat("        ---------------------------------------    \n")
cat("Call:\n")
print(x$call)
cat("\nResultant Bayesian ",x$Optcrit,"-optimal block design:\n",sep="")
cat("\n")
print(data.frame(x$OptdesF))
cat("\n", x$file_loc2,"\n", x$file_loc,"\n")
cat("\n")
}
