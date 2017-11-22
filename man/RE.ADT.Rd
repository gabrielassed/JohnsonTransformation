\name{RE.ADT}
\alias{RE.ADT}

\title{Anderson-Darling test}

\description{RE.ADT performs the Anderson-Darling test according to Trujillo-Ortiz(2007)}
\usage{
RE.ADT(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x  vector of observations
}
}
\details{ 
For only work with p-values see the second example
}
\value{
The object returned consists of the following items:
p-value      the resulting p-value of the transformation
}
\references{Trujillo-Ortiz, A., R. H.-W. K. B.-R. & Castro-Perez., A.(2007),
 "Andartest:anderson-darling test for assessing
normality of a sample data.".
URL: http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=14807
}
\author{Edgar Santos Fernandez}
\note{
The function RE.ADT use the traditional algorithm while ADGofTest package is based on the Marsaglia approach.
}


\seealso{
<pkg>
}
\examples{
# performing the AD test for a random sample
x <- rnorm(100,10,2)
y <- RE.ADT(x); print(y)

#working with the p-value
x <- runif(100)
y <- RE.ADT(x) $p; print(y)

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
