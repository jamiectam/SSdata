#' @title Calculates Shannon's Index of Diversity
#'@description This function calculates Shannon's Index of Diversity for \eqn{j}
#'  areas and \eqn{i} years.
#'@details Shannon's index of diversity (H'): \deqn{H' = -\Sigma p_i ln(p_i)}
#'  \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'  species and \eqn{S} is the number of species recorded in the sample. This
#'  index is sensitive to the number of species recorded in the sample
#'  (Magurran, 1988).
#'@param X A dataframe of fishery independent survey data or model output with
#'  columns \code{YEAR}, \code{ID}, \code{SPECIES}, and \code{ABUNDANCE}.
#'  \code{YEAR} indicates the year the observation was recorded, \code{ID} is an
#'  area code indicating where the observation was recorded, \code{SPECIES} is a
#'  numeric code indicating the species sampled, and \code{ABUNDANCE} is the
#'  corresponding abundance (stratified and corrected for catchability as
#'  required).
#'@param group A character string indicating which species to include in the
#'  indicator calculation. If \code{group = "ALL"}, all species will be
#'  included; otherwise, \code{group} should match a column name in
#'  \code{species.table}.
#'@param species.table A table with at least one column, where the column name
#'  is the string \code{group}, and the column entries are species codes from
#'  \code{X} indicating which species are included that group.
#'  \code{species.table} may also include columns for other species groups;
#'  these will be ignored. If \code{group = "ALL"}, this table is not required.
#'  Default is \code{species.table = NULL}.
#'@param metric A character string indicating which column in \code{X} to use to
#'  calculate the indicator. Default is \code{metric = "ABUNDANCE"}.
#'@param years A vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'  \code{ShannonDiversity}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of 

loadfun <- function(path) {
  a <- dir(file.path(path,'R'))
  for(i in 1:length(a)) {
    source(file.path(path,'R',a[i]))
  }
}