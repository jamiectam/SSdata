#'@title Assigns area ID based on strata. 
#'@description Adds column \code{ID} to a dataframe that already has a column
#'  \code{STRAT}, and then assigns an area ID based on the strata number in
#'  \code{STRAT}.
#'@param dat A dataframe that includes the column \code{STRAT}, which has
#'  entries corresponding to the strata on the Scotian Shelf (e.g., values from
#'  440 to 495).
#'@param area Character string indicating the spatial scale for which to assign
#'  areas IDs. Options are: \code{"strat"}, \code{"nafo"}, \code{"esswss"},
#'  \code{"shelf"}, \code{"indiseas"}, \code{"pollockwest"},
#'  \code{"pollockeast"}, \code{"unit2redfish"}, \code{"unit3redfish"},
#'  \code{"silverhake"}.
#'@return The function returns \code{dat} with an extra column: \code{ID}
#'@references Modified code from AC's ExtractIndicators/R/defineGroups.R.
#'@export

defineAreas <- function(dat, area) {
	if(area == "nafo") {
	ar <- data.frame(STRAT = c(440:466,470:478,480:485,490:495),
	                 ID = c(rep('4VN',3), rep('4VS',10), rep('4W',14), rep('4X',21)))
	}
	if(area == "shelf") {
	ar <- data.frame(STRAT = c(440:466,470:478,480:485,490:495),
	                 ID = rep('SHELF',48))
	}
	if(area == "esswss") {
	ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),
	                 ID = c(rep('ESS', 27),rep('WSS', 21)))
	}
	if(area == "indiseas") {
	ar <- data.frame(STRAT = c(443:466,470:478,480:485,490:495),
	                 ID = c(rep('ESS',24),rep('WSS',21)))
	}
	if(area == "strat") {
	ar <- data.frame(STRAT = c(440:466,470:478,480:485,490:495),
	                 ID = c(440:466,470:478,480:485,490:495))
	}
	if(area == "pollockwest") {
	ar <- data.frame(STRAT = c(474,476, 480, 481, 482,483,484,485,486,487,488,489,490,491,492,493,494,495),
	                 ID = c('pollockwest'))
	}
	if(area == "pollockeast") {
	ar <- data.frame(STRAT=c(440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,475,477,478),
	                 ID = c('pollockeast'))
	}
	if(area == "unit2redfish") {
	ar <- data.frame(STRAT = c(440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,464),
	                 ID = c('U2redfish'))
	}
	if(area == "unit3redfish") {
	ar <- data.frame(STRAT = c(457,458,459,460,461,462,463,465,466,467,468,469,470,471,472,473,474,475,476,477,478,480,481,482,483,484,485),
	                 ID = c('U3redfish'))
	}
	if(area == "silverhake") {
	ar <- data.frame(STRAT = c(440:483),
	                 ID = c('silverhake'))
	}

	if(any(!is.na(dat$STRAT))) {
	dat <- merge(dat, ar, by='STRAT', all.x=T)
	} else {
	print('Data input problems, you need to identify a strata column from summer rv strata and a group (area) which defines the group (nafo,shelf,esswss or shelf)')
	dat <- dat
	}
	dat
}

