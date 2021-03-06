% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold_drop.R
\name{threshold_drop}
\alias{threshold_drop}
\title{Threshold drop}
\usage{
threshold_drop(OTUtab, threshold)
}
\arguments{
\item{OTUtab}{maticový zápis OTU tabulky}

\item{threshold}{číselná proměnná. Prahová hodnota se kterou jsou pozorování porovnávána.}
}
\value{
maticový zápis redukované OTU tabulky
}
\description{
Funkce threshold_drop redukuje OTU tabulku na základě nastaveného práhu threshold. Pozorování nižší než nastavený práh jsou
z tabulky odstraněna.
}
