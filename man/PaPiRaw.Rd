\name{PaPiRaw}
\alias{PaPiRaw}
\docType{data}
\title{
PaPiRaw
}
\description{
A dataframe with 44 series of wood density (\ifelse{html}{\out{g/m<sup>3</sup>}}{\eqn{g/m^3}}).
}
\usage{data("PaPiRaw")}
\format{
  A data.frame containing 44 series in columns and 3111 values of wood density in rows.
%%  \describe{
%%    \item{\code{x}}{a numeric vector}
%%    \item{\code{y}}{a numeric vector}
%%  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
data(PaPiRaw)
plot(na.omit(PaPiRaw[,1]), type="l", ann = FALSE)
}
\keyword{datasets}
