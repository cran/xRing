\name{stepIncrease}
\alias{stepIncrease}
\title{Calculate the Steps Thickness of the Calibration Wedge}
\usage{
stepIncrease(step.increase = 0.24, nsteps = 7)
}
\arguments{
\item{step.increase}{height increase per wedge step}

\item{nsteps}{total number of steps (the first step has the thickness of 0 - the area beside the wedge. Mention that when setting nsteps)}
}
\value{
a numeric vector
}
\description{
convenience function to calculate the thickness of each steps of the calibration wedge for wedges with continous step increase.
}
