% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFigshareInfo.R
\name{getFigshareInfo}
\alias{getFigshareInfo}
\title{getFigshareInfo}
\usage{
getFigshareInfo(token, id)
}
\arguments{
\item{token}{Personal API token from users Figshare account, see
\href{https://help.figshare.com/article/how-to-get-a-personal-token}{here}
for information on creating a token}

\item{id}{Figshare article ID to download information for}
}
\value{
dataframe with columns \code{filename} and \code{recording_url}
}
\description{
downloads filename and recording URL information from a Figshare
  article. Requires a users API token from their figshare account
}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
