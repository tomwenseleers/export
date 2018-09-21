\name{wine.rda}
\alias{wine.rda}
\alias{wine}
\alias{wine.class}
\docType{data}
\title{
Wine data set
}
\description{
These data are the results of a chemical analysis of wines grown in the same region in Italy 
but derived from three different cultivars. The analysis determined the quantities of 13 
constituents found in each of the three types of wines. 
}
\format{
  The format is an Rda object containing 2 variables 
  \itemize{
    \item \code{wine}: a data frame of class \code{data.frame} containing the variables:
      \enumerate{
        \item Alcohol 
        \item Malic acid 
        \item Ash 
        \item Alcalinity of ash 
        \item Magnesium 
        \item Total phenols 
        \item Flavanoids 
        \item Nonflavanoid phenols 
        \item Proanthocyanins 
        \item Color intensity 
        \item Hue 
        \item OD280/OD315 of diluted wines 
        \item Proline 
    }
   \item \code{wine.class}: a vector of factors giving the class (barolo, grignolino, barbera) of each wine.
  }
}
\source{
The dataset was taken from the UCI repository https://archive.ics.uci.edu/ml/datasets/wine
Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. 
Irvine, CA: University of California, School of Information and Computer Science.
}
\references{
Original Owners: 
Forina, M. et al, PARVUS - An Extendible Package for Data Exploration, Classification and Correlation. 
Institute of Pharmaceutical and Food Analysis and Technologies, Via Brigata Salerno, 16147 Genoa, Italy. 
Donor: 
Stefan Aeberhard, email: stefan@coral.cs.jcu.edu.au
Using chemical analysis determine the origin of wines
}
\keyword{datasets}
