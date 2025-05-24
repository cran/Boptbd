\name{graphoptBbd}
\alias{graphoptBbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates the graphical layout of resultant optimal design
}
\description{
The function \code{graphoptBbd} creates the graphical layout of resultant Bayesian A-, or D-optimal block design on a separate pop-up GUI tcltk window.
}
\usage{
graphoptBbd(trt.N, blk.N, alpha, beta, OptdesF, Optcrit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt.N}{
integer, specifying  number of treatments, \code{v}. 
}
  \item{blk.N}{
integer, specifying  number of blocks, \code{b}.
}
  \item{alpha}{
numeric, representing  parameter of beta distribution. 
}
  \item{beta}{
numeric, representing  parameter of beta distribution.
}
  \item{OptdesF}{
matrix, a \code{2 x b} obtained Bayesian optimal block design.
}
  \item{Optcrit}{
character specifying the optimality criteria to be used. Thus, \code{Optcrit} takes the letter \code{"A"} and \code{"D"} for Bayesian \code{A-} and \code{D-}optimal block designs, respectively.
}
}
\details{
Detail discussions  concerning the constructions of a graphs can be found in \code{igraph} R package.
}
\value{
Returns the graphical layout of the resultant Bayesian optimal block design '\code{OptdesF}' on a separate pop-up window. Furthermore, the function saves the graphical layout of the resultant Bayesain optimal block design in .pdf format in a temporary directory. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Dibaba Bayisa Gemechu, Legesse Kassa Debusho, and Linda Haines
}
\note{
When closing a pop-up window for graphical layout of the resultant designs (Graph plot),  if  the window is closed by 
clicking on the red button with "X" sign (top-right), the warning message  "Warning message: In \code{rm(list = cmd,} \code{envir = .tkplot.env):} object 'tkp \dots' not found"
will occur in R-console irrespective of what command is executed next. To resolve this warning message, click 
on "\code{close}" menu that is located at the top-left of the graph plot pop-up window when closing this window. 
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{igraph}
}
\examples{
    ##To create the graphical layout of the D-optimal block design
    ##obtained using the treatment exchange algorithm for 
    trt.N <- 10  #Number of treatments
    blk.N <- 10  #Number of blocks
    alpha <- 0.1 #alpha value 
    beta  <- 0.1 #beta value 
    OptdesF <- rbind(1:10, c(2:10,1)) #Bayesian A-optimal block design (loop design)

    graphoptBbd(trt.N = 10, blk.N = 10, alpha = 0.1, beta = 0.1, OptdesF, Optcrit = "A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Graphical layout}
%\keyword{Optimal block design}% __ONLY ONE__ keyword per line 
