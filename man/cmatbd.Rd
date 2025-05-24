\name{cmatbd}
\alias{cmatbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the treatment information matrix 
}
\description{
The function \code{cmatbd} computes the information matrix (C-matrix) for treatment effects under either the linear fixed effects model  or the linear mixed effects model setting for a given block design of size 2.
}
\usage{
cmatbd(trt.N, blk.N, theta, des)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trt.N}{
integer, specifying number of treatments, \code{v}. 
}
  \item{blk.N}{
integer, specifying number of blocks, \code{b}.
}
  \item{theta}{
numeric, representing  function of the ratio of random block variance and random error variance. It takes any value between 0 and 1, inclusive. 
}
  \item{des}{
matrix, a \code{2 x b} block design with \code{b} blocks of size \code{k = 2} and \code{v} treatments.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a \code{v x v} treatment information matrix (C-matrix).}
\references{
Debusho, L. K., Gemechu, D. B. and Haines, L. (2018). Algorithmic construction of optimal block designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Communications in Statistics - Simulation and Computation, https://doi.org/10.1080/03610918.2018.1429617}.

Gemechu D. B., Debusho L. K. and Haines L. M. (2025). Algorithmic Construction of Bayesian Optimal Block Designs Using the Linear Mixed Effects Model. \emph{International Journal of Statistics and Probability. 14 (1), 1-50, DOI:10.5539/ijsp.v14n1p50}.

Gemechu D. B., Debusho L. K. and Haines L. M. (2014). A-optimal designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Peer-reviewed Proceedings of the Annual Conference of the South African Statistical Association for 2014 (SASA 2014), Rhodes University, Grahamstown, South Africa}. pp 33-40, ISBN: 978-1-86822-659-7.
}
\author{
Dibaba Bayisa Gemechu, Legesse Kassa Debusho, and Linda Haines
}

\seealso{
\code{\link{Boptbd}}, \code{\link{fixparBbd}}, \code{\link{intcbd}}
}
\examples{

##Information matrix

     trt.N <- 3 

     blk.N <- 3 

     theta <- 0.2 

     des <- intcbd(trt.N = 4, blk.N = 3)

     cmatbd(trt.N, blk.N, theta, des)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Information matrix}
\keyword{C-matrix} 
