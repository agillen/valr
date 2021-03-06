% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorting.r
\name{sorting}
\alias{sorting}
\title{Sorting tbls of intervals}
\description{
Interval tbls can be sorted using \code{\link[dplyr:arrange]{dplyr::arrange()}}. See examples
for sorting by chrom and start, and by size using
\code{\link[dplyr:mutate]{dplyr::mutate()}}.
}
\examples{

# unsorted tbl
x <- trbl_interval(
  ~chrom, ~start, ~end,
  'chr1', 150,    500,
  'chr1', 1,      100,
  'chr2', 500,    1000,
  'chr2', 100,    300
) 

# sort by start
dplyr::arrange(x, start)

# sort by descending start
dplyr::arrange(x, desc(start))

# sort by chrom and start
dplyr::arrange(x, chrom, start)

# sort by size
x <- dplyr::mutate(x, .size = end - start)
dplyr::arrange(x, .size)

}
