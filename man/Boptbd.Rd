\name{Boptbd}
\alias{Boptbd}
\alias{Boptbd.default}
\alias{print.Boptbd}
\alias{summary.Boptbd}
\alias{print.summary.Boptbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesain optimal block designs 
}
\description{
The function \code{Boptbd} is used to compute Bayesian A- or D-optimal block designs under the linear mixed effects model settings using array/block exchange algorithm of Debusho, Gemechu and Haines (2018).}
\usage{
Boptbd(trt.N, blk.N, alpha, beta, nrep, brep, itr.cvrgval, Optcrit = "", ...)

\method{Boptbd}{default}(trt.N, blk.N, alpha, beta, nrep, brep, itr.cvrgval, Optcrit = "", ...)
\method{print}{Boptbd}(x, ...)
\method{summary}{Boptbd}(object, ...)
}
\arguments{
  \item{trt.N}{
integer, specifying number of treatments, \code{v}. 
}
  \item{blk.N}{
integer, specifying number of blocks, \code{b}.
}
  \item{alpha}{
numeric, representing  the shape parameter of beta distribution. 
}
  \item{beta}{
numeric, representing  the shape parameter of beta distribution.
}
  \item{nrep}{
integer, specifying number of replications of the optimization procedure. 
}
  \item{brep}{
integer, specifying number of Monte Carlo samples from a prior beta distribution. 
}
  \item{itr.cvrgval}{
integer, specifying number of iterations required for convergence during the block exchange procedure. 
}
  \item{Optcrit}{
character, specifying the optimality criteria to be used. \code{Optcrit} takes the letter \code{"A"} and \code{"D"} for Bayesian \code{A-} and \code{D-}optimal block designs, respectively.
}
  \item{x}{
the object to be printed.
}
  \item{object}{
an object of class \code{"Boptbd"}.
}
  \item{\dots}{
not used.
}
}
\details{
\code{Boptbd} computes Bayesian optimal block designs 
where the interest is in a comparison of all possible elementary treatment contrasts. Under the linear mixed effects model setting,
where the block effects are assumed to be random, the treatment information matrix (C-matrix) is dependent on the unknown parameter \code{rho} (ratio of unknown
variance components of random error and block effects). A Bayesian optimal design extends the locally optimal approach by specifying a prior distribution for the parameter \code{rho}.  \code{Boptbd} function computes Bayesian \code{A-} and \code{D-}optimal 
block designs via calling of two sub-functions \code{\link{Baoptbd}} and \code{\link{Bdoptbd}}, respectively. Each function requires an initial connected block designs 
generated using the function \code{\link{intcbd}}.  

The minimum value of \code{trt.N} and \code{blk.N} is 3 and \code{trt.N} should be less than or equal to \code{blk.N - 1}. 

\code{Boptbd} perform the block exchange procedure through deletion and addition of candidate block at a time and selects a 
design with best block exchange with respect to the optimality criterion value. It uses the steps of Bueno Filho and Gilmour (2007) for numerical evaluation of the
Bayesian criterion values.

\code{nrep} takes a value of greater than or equal to 2. However, to ensure optimality of the resultant design, 
the \code{nrep} should be greater than or equal to 10 and in addition, as \code{trt.N} and \code{blk.N} increase, 
to ensure optimality of resultant design, it is advised to further increase the value of \code{nrep}
up to greater than or equal to 100. \code{brep} takes a value of greater than or equal to 2. 
As \code{brep} value increase, the execution time to generate Bayesian optimal design increase.  

\code{itr.cvrgval} number of iterations during exchange procedure. It takes a value between 2 and \code{blk.N}. It is used 
to speedup the computer search time by setting how long should the user should wait for the exchange process to obtain any 
different (if any) design than the one that was produced as the result of the preceding exchange of the current array in the initial 
design with candidate array. This is mainly effective if \code{blk.N} is very large. For example \code{itr.cvrgval = 2}, means the 
exchange procedure will jump to the next block test if the exchange of the two preceding blocks with candidate block results with the 
same efficient designs. The function  will not give error message if the users set \code{itr.cvrgval > blk.N} and it will automatically 
set \code{itr.cvrgval = blk.N}. The smaller the \code{itr.cvrgval} means the faster the exchange procedure is, but this will reduce the 
chance of getting optimal block design and users are advised to set \code{itr.cvrgval} closer to \code{blk.N}. 

}
\value{
Returns the resultant Bayesian A- or  D-optimal block design with its corresponding score value and parametric combination 
saved in excel file in a temporary directory. In addition, the function \code{Boptbd} displays the graphical layout of the resultant Bayesian
optimal block designs. Specifically: 

\item{call}{the method call.}         
\item{v}{number of treatments.}
\item{b}{number of blocks}
\item{alpha}{alpha value.}
\item{beta}{beta value.}
\item{nrep}{number of replications of the optimization procedure.}  
\item{itr.cvrgval}{number of iterations required for convergence during the exchange procedure.}                          
\item{Optcrit}{optimality criteria.}                  
\item{brep}{umber of Monte Carlo samples from a prior beta distribution.}
\item{OptdesF}{a \code{2 x b} obtained Bayesain optimal block design.}
\item{Optcrtsv}{score value of the optimality criteria \code{'Optcrit'} of the resultant Bayesian optimal block design \code{'OptdesF'}.}
\item{file_loc, file_loc2}{location where the summary of the resultant Bayesian optimal block design  is saved in .csv format.}
\item{equireplicate}{logical value indicating whether the resultant Bayesian optimal block design  is equireplicate or not.}
\item{vtrtrep}{vector of treatment replication of the resultant Bayesian optimal block design.}     
\item{Cmat}{the C-matrix or  treatment information matrix of the Bayesian optimal block design.} 

The graphical layout of the resultant Bayesain optimal block design.   

NB: The function "Boptbd" also saves the summary of the resultant Bayesian optimal block design in .csv format in a temporary directory. 
Furthermore, this function reports only one final optimal block design, however, there is a possibility 
of more than one optimal block designs for a given parametric combination. 
The function \code{\link{graphoptBbd}} can be used to view and rearrange the graphical layout of the resultant 
optimal block design on \code{tcltk} window. Alternative to the function \code{Boptbd}, a
GUI tcltk window can be used to generate Bayesain optimal block designs, see \code{\link{mmenuBbd}} and \code{\link{fixparBbd}}.   

}
\references{
Bueno Filho, J. S. de S., Gilmour, S. G. and Rosa, G. J. M. (2006). Design of microarray experiments for genetical genomics studies. Genetics, 174, 945-957

Debusho, L. K., Gemechu, D. B. and Haines, L. (2018). Algorithmic construction of optimal block designs for two-colour cDNA microarray experiments using the linear mixed effects model. \emph{Communications in Statistics - Simulation and Computation, https://doi.org/10.1080/03610918.2018.1429617}.

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
\code{\link{mmenuBbd}}, \code{\link{fixparBbd}}, \code{\link{intcbd}}
}
\examples{
  ##To obtain Bayesian A-optimal block design for the following treatment combintions:
  trt.N <- 3 #Number of treatments
  blk.N <- 3 #Number of blocks
  alpha <- 0.1 #alpha value
  beta <- 0.1 #beta value
  nrep <- 5  #Number of replications
  brep <- 5  #Number of Monte Carlo samples from a prior beta distribution, Beta(0.1, 0.1) 
  itr.cvrgval <- 6 #Number of iterations required during the exchange procedure
  Optcrit <- "A"   #Optimality criteria
  
  Baoptbd_example <- Boptbd(trt.N = 3, blk.N = 3, alpha = 0.1, beta = 0.1, nrep = 5, brep = 5, 
                            itr.cvrgval = 6, Optcrit = "A")
  
  summary(Baoptbd_example)
}

\keyword{Bayesian A-optimal block designs}
\keyword{Bayesain D-optimal block designs}
\keyword{Block/array exchange algorithm} 
