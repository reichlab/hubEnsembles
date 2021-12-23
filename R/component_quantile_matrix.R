
new_cqm <- function(cqm, quantiles = rownames(cqm), components = colnames(cqm)) {
	cqm = structure(
		cqm,
		quantiles = as.numeric(quantiles),
		quantiles_formatted = format(quantiles, digits = 3, nsmall = 3),
		components = components,
		class = c("cqm", "matrix")
	)
	cqm <- validate_cqm(cqm)
	return(cqm)
}