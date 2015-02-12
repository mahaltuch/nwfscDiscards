\name{Convert_discardLF_to_ss3_format_Fn}
\alias{Convert_discardLF_to_ss3_format_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert length-frequency of expanded discards to SS3 format
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Convert_discardLF_to_ss3_format_Fn(Length_Frequency_Table, LengthBins, season = 1, fleet = NA, gender = 0, partition = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Length_Frequency_Table}{
%%     ~~Describe \code{Length_Frequency_Table} here~~
}
  \item{LengthBins}{
%%     ~~Describe \code{LengthBins} here~~
}
  \item{season}{
%%     ~~Describe \code{season} here~~
}
  \item{fleet}{
%%     ~~Describe \code{fleet} here~~
}
  \item{gender}{
%%     ~~Describe \code{gender} here~~
}
  \item{partition}{
%%     ~~Describe \code{partition} here~~
}
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Length_Frequency_Table, LengthBins, season = 1, fleet = NA, 
    gender = 0, partition = 1) 
{
    Year_Set = sort(unique(Length_Frequency_Table[, "Year"]))
    RowUsedTF = rep(FALSE, nrow(Length_Frequency_Table))
    Matrix = matrix(0, nrow = length(Year_Set), ncol = 6 + 2 * 
        length(LengthBins), dimnames = list(NULL, c("Year", "Season", 
        "Fleet", "Gender", "Partition", "Nsamp", paste0("F_", 
            LengthBins), paste0("M_", LengthBins))))
    Return = data.frame(Matrix)
    for (yI in 1:length(Year_Set)) {
        Return[yI, c("Year", "Season", "Fleet", "Gender", "Partition")] = c(Year_Set[yI], 
            season, fleet, gender, partition)
        for (lI in 1:length(LengthBins)) {
            RowNum = which(Length_Frequency_Table[, "Year"] == 
                Year_Set[yI] & Length_Frequency_Table[, "Lenbin"] == 
                LengthBins[lI])
            if (length(RowNum) >= 2) 
                stop("Check Length_Frequency_Table for duplicated rows")
            if (length(RowNum) == 1) {
                Return[yI, paste0("F_", LengthBins[lI])] = Length_Frequency_Table[RowNum, 
                  "Prop.numbers"]
                RowUsedTF[RowNum] = TRUE
            }
        }
    }
    if (any(RowUsedTF == FALSE)) {
        stop(paste("Rows", which(RowUsedTF == FALSE), "not used"))
    }
    if (sum(Return[, paste0("F_", LengthBins)]) != sum(Length_Frequency_Table[, 
        "Prop.numbers"])) {
        stop("totals don't match")
    }
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
