\name{FancyPack.R.LiU.3-package}
\alias{FancyPack.R.LiU.3-package}
\alias{FancyPack.R.LiU.3}
\docType{package}
\title{
\packageTitle{FancyPack.R.LiU.3}
}
\description{
\packageDescription{FancyPack.R.LiU.3}
}
\details{
Two functions included: euclidean and dijkstra functions
}
\author{
\packageAuthor{FancyPack.R.LiU.3}

Maintainer: \packageMaintainer{FancyPack.R.LiU.3}
}
\references{
https://en.wikipedia.org/wiki/Euclidean_algorithm

https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
}

\keyword{ package }
\seealso{
N/A
}
\examples{
euclidean(100, 1000)
wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)  
}
