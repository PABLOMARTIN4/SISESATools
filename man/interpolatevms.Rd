\name{interpolatevms}
\alias{interpolatevms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interpolate georeference points
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interpolatevms(datsat, interval = 60, proj4 = NULL, area = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datsat}{datsat dataset with georeference points (longitude y latitude) that need to be interpolated}
  \item{interval}{Time bewteem each point interpolate}
  \item{proj4}{...}
  \item{area}{...}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Pablo Marin and Rocío Joo.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{xxx}},
         \code{\link{xxx}}}

\examples{

# Load the datsat data
data(datsat)

datsat = datsat[datsat$trip == 1,]

# Interpolate the VMS data
interpolation <- interpolatevms(datsat, interval = 60, proj4 = NULL, area = NULL)

# Map
mapVMS(x = datsat$lon, y = datsat$lat,  speed = datsat$Vel.Cal)
points(interpolation$lon2, intepolation$lat2, pch = 16, cex = 0.8)

}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
