\name{Boptbd-internal}
\alias{Boptbd-internal}
\alias{Baoptbd}
\alias{Bdoptbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function
}
\description{
This function is for internal usage only
}
\usage{
 
## Computes Bayesian A-optimal block designs
## using block/array exchange algorithm
Baoptbd(trt.N, blk.N, alpha, beta, nrep, brep, itr.cvrgval)
 
## Computes Bayesian D-optimal  block designs
## using block/array exchange algorithm
Bdoptbd(trt.N, blk.N, alpha, beta, nrep, brep, itr.cvrgval)
}
\arguments{
  \item{trt.N}{
integer, specifying number of treatments, \code{v}. 
}
  \item{blk.N}{
integer, specifying number of arrays, \code{b}.
}
  \item{alpha}{
numeric, representing  shape parameter of beta distribution. 
}
  \item{beta}{
numeric, representing  shape parameter of beta distribution.
}
  \item{nrep}{
integer, specifying number of replications of the optimization procedure. 
}
  \item{brep}{
integer, specifying number of Monte Carlo samples from a prior beta distribution, Beta(alpha, beta). 
}
  \item{itr.cvrgval}{
integer, specifying number of iterations required for convergence during the exchange procedure. See \code{\link{Boptbd}} documentation for details.
}
}
\details{
These functions are handled via a generic function \code{\link{Boptbd}}. Please refer to the \code{\link{Boptbd}} documentation for details. 
}

\references{
Debusho, L. K., Gemechu, D. B. and Haines, L. (2018). Algorithmic construction of optimal block designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Communications in Statistics - Simulation and Computation, https://doi.org/10.1080/03610918.2018.1429617}.

Gemechu D. B., Debusho L. K. and Haines L. M. (2025). Algorithmic Construction of Bayesian Optimal Block Designs Using the Linear Mixed Effects Model. \emph{International Journal of Statistics and Probability. 14 (1), 1-50, DOI:10.5539/ijsp.v14n1p50}.

Gemechu D. B., Debusho L. K. and Haines L. M. (2014). A-optimal designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Peer-reviewed Proceedings of the Annual Conference of the South African Statistical Association for 2014 (SASA 2014), Rhodes University, Grahamstown, South Africa}. pp 33-40, ISBN: 978-1-86822-659-7.
}
\author{
Dibaba Bayisa Gemechu, Legesse Kassa Debusho, and Linda Haines
}
%\note{
%The detail description of the functions and the way it work can be obtained from different sources (working manual).
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Boptbd}}
}
%%\examples{
%%To start up the TCL/Tk window
%%optblkd.mae()
%%}
\keyword{Bayesian A-optimal block designs}
\keyword{Bayesian D-optimal block designs}
