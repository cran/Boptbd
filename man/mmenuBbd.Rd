\name{mmenuBbd}
\alias{mmenuBbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates the main menu tcltk window
}
\description{
Alternative to directly using the function \code{\link{Boptbd}} on the R console, the function \code{mmenuBbd} creates the main menu tcltk window with the list of two optimality criteria (A and D) for search of Bayesian optimal block designs.}
\usage{
mmenuBbd()
}
%- maybe also 'usage' for other objects documented here.
\details{
The main menu window created using the function \code{mmenuBbd} contains the list of two optimality criteria (the A- and D) for block designs that allows user to choose an optimality criterion of interest. The function 
will then call for function \code{fixpaBrbd} for further option regarding the setup of parametric combinations and output, see \code{fixparBbd} documentation. 
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Dibaba B. Gemechu, Legesse K. Debusho, and Linda M. Haines
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Boptbd}}, \code{\link{fixparBbd}}, \code{\link{tcltk}}, \code{\link{TkWidgets}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line 
