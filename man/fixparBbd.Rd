\name{fixparBbd}
\alias{fixparBbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets parametric values
}
\description{
The function \code{fixparbd} creates a \code{\link{tcltk}} widow that allows users to set or fix values for the parametric combinations to compute Bayesian optimal block designs.}
\usage{
fixparBbd(Optcrit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Optcrit}{
character, specifying the optimality criteria to be used. \code{Optcrit} takes the letter \code{"A"} and \code{"D"} for Bayesian \code{A-} and \code{D-} optimal block designs, respectively.
}
}
\details{
\code{fixparBbd} creates a pop-up tcltk window that allows users to set 
the parametric combinations to compute optimal or near-optimal block designs. The parameters include the number of treatments, number blocks, 
alpha and betha value, number of replications of the optimization procedure \code{nrep}, number of Monte Carlo samples \code{brep}  and number of iterations required during exchange procedure \code{itr.cvrgval}. 
Furthermore, on this window, the checkbox options that allow the users to choose whether or not they need to have the graphical 
layout of the resultant Bayesian optimal block design and to print the summary of the resultant Bayesian optimal design on 
R-console directly are available. 

After setting all the required parametric combinations and selecting the algorithm of interest, 
clicking on the search button on the set parametric combinations tcltk window, similar to the results that 
can be obtained when using the function \code{\link{Boptbd}}, the summary of the resultant Bayesian optimal
block design is also saved in a temporary directory in .csv format and it will also be displayed on R console with 
graphical layout of the resultant Bayesian optimal block designs.
}
\value{
The \code{fixparBbd} function creates a pop-up tcltk window that allows users to set 
the parametric combinations to compute Bayesian optimal block designs.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Dibaba Bayisa Gemechu, Legesse Kassa Debusho, and Linda Haines
}
%\note{
%%  ~~further notes~~
%}
%
%% ~Make other sections like Warning with \section{Warning }{....} ~
 
\seealso{
\code{\link{Boptbd}}, \code{\link{mmenuBbd}}, \code{\link{tcltk}}, \code{\link{TkWidgets}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line 
 