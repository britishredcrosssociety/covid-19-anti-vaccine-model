#' Calculate risk quantiles
#'
#' @param risk.col The data to quantise
#' @param quants Number of quantiles (default: 5)
#' @param highest.quantile.is.worst Should a risk score of 1 represent the highest/worst number in the data (FALSE) or the lowest/best (FALSE)?
#' @param style Method to use for calculating quantiles (passed to classIntervals; default: Fisher). One of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks" or "dpih"
#' @param samp_prop The proportion of samples to use, if slicing using "fisher" or "jenks" (passed to classIntervals; default: 100%)
#' @return A vector containing the risk quantiles
#'
calc_risk_quantiles <- function(risk.col, quants = 5, highest.quantile.is.worst = TRUE, style = "fisher", samp_prop = 1) {
  if (length(unique(risk.col)) > 1) { # only calculate quintiles if there are more unique values than quantiles

    # calculate the quantile breaks
    q_breaks <- classInt::classIntervals(risk.col, quants, style = style, samp_prop = samp_prop, largeN = length(risk.col))

    q <- as.integer(cut(risk.col, breaks = q_breaks$brks, include.lowest = T)) # create a column with the risk quantiles as a number (e.g. from 1 to 5, if using quintiles)

    if (!highest.quantile.is.worst) {
      max_quant <- max(q, na.rm = TRUE) # get the max. quantile in the dataset (won't always be equal to `quants`, e.g. if nrows(d) < quants)
      q <- (max_quant + 1) - q # reverse the quantile scoring so 1 = highest risk
    }

    q # return the quantiles
  } else {
    1
  }
}

#' Normalise ranks to a range between 0 and 1
#'
#' @param x List of ranks
#'
scale_ranks <- function(x) (x - 1) / (length(x) - 1)

#' Rank indicators but put NAs first (i.e. least-worst)
#' @param x Data to rank
#'
rank2 <- function(x) rank(x, na.last = FALSE)

#' Inverse ranking with NAs first (i.e. 1 = worst)
#' @param x Data to rank
#'
inverse_rank <- function(x) (length(x) + 1) - rank(x, na.last = FALSE)


# ---- Calc Vaccine Score Function ----
calc_vaccine_scores <-
  function(data) {

    # Rank inputs
    data <- data %>%
      mutate(
        compliance_score_rank = rank2(compliance_score),
        dep_score_rank = rank2(dep_score),
        income_score_rank = rank2(income_score),
        perc_female_rank = rank2(perc_female),
        vac_rate_rank = inverse_rank(vac_rate)
      )

    # Scale the ranked inputs
    data <- data %>%
      mutate_at(vars(ends_with("rank")), list(scaled = scale_ranks))

    # Multiply by RRR weights
    # Take the mean of the willingness and undecided RRR for the respective
    # indicators
    # Table 3 from: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3716874
    data <- data %>%
      mutate(
        compliance_score_weighted = compliance_score_rank_scaled * mean(c(1.60, 1.77)),
        dep_score_weight = dep_score_rank_scaled * mean(c(1.38, 1.60)),
        # CACI Technical Guide states the income measure is based on households
        # with an income of under £20,000 per year. Take a mean if both of the
        # lowest bracket bands to capture this group
        income_score_weighted = income_score_rank_scaled * mean(c(1.47, 1.59, 2.10, 2.16)),
        perc_female_weighted = perc_female_rank_scaled * mean(c(1.45, 1.52)),
        vac_rate_weighted = vac_rate_rank_scaled * mean(c(1.93, 3.40))
      )

    # Sum weighted indicators
    data <- data %>%
      mutate(`Vulnerability score` = reduce(select(., ends_with("_weighted")), `+`))

    # Rank and quantise into deciles
    data <- data %>%
      mutate(`Vulnerability rank` = rank(`Vulnerability score`)) %>%
      mutate(`Vulnerability decile` = calc_risk_quantiles(`Vulnerability rank`, quants = 10))

    # Return data
    return(data)
  }
