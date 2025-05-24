\name{intcbd}
\alias{intcbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates initial connected block design
}
\description{
The function \code{intcbd} generates a random initial connected block design for a given number of blocks \code{b} of size \code{k = 2} and the number of treatments \code{v}.}
\usage{
intcbd(trt.N, blk.N)
}

\arguments{
 \item{trt.N}{
integer, specifying number of treatments, \code{v}. 
}
  \item{blk.N}{
integer, specifying number of arrays, \code{b}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a \code{2 x b} connected block design with \code{b} blocks of size \code{k = 2} and number of treatments \code{v}. 
}
\references{
Debusho, L. K., Gemechu, D. B., and Haines, L. M. (2018).  Algorithmic construction of optimal block designs for two-colour cDNA microarray experiments using the linear mixed model.

Gemechu D. B., Debusho L. K. and Haines L. M. (2025). Algorithmic Construction of Bayesian Optimal Block Designs Using the Linear Mixed Effects Model. \emph{International Journal of Statistics and Probability. 14 (1), 1-50, DOI:10.5539/ijsp.v14n1p50}.

Gemechu D. B., Debusho L. K. and Haines L. M. (2014). A-optimal designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Peer-reviewed Proceedings of the Annual Conference of the South African Statistical Association for 2014 (SASA 2014), Rhodes University, Grahamstown, South Africa}. pp 33-40, ISBN: 978-1-86822-659-7.
}
\author{
Dibaba Bayisa Gemechu, Legesse Kassa Debusho, and Linda Haines
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Boptbd}}, \code{\link{cmatbd}}
}
\examples{
    #Initial connected block design for 
    trt.N <- 4 #Number of treatments
    blk.N <- 4 #Number of blocks

    intcbd(trt.N = 4, blk.N = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Initial block design}
\keyword{Connected block design}% __ONLY ONE__ keyword per line
