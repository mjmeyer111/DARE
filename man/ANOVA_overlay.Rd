% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ANOVA_overlay.R
\name{ANOVA_overlay}
\alias{ANOVA_overlay}
\title{Splitting your data to construct overlays that mimic the four sections of
data used to calculate a one-way ANOVA.}
\usage{
ANOVA_overlay(x, ...)
}
\arguments{
\item{x}{Either your independent variable (as a character), or a formula
in the form dv ~ iv, where dv is your dependent variable, and iv is your
independent variable.}

\item{y}{Character: What is the name of your dependent variable?}

\item{data}{tibble or data frame: Your dataset containing your variables.}

\item{common}{function: which function should represent the common in the
splitting algorithm?}

\item{print.statistic}{Logical: should the test statistic be printed in the
source table?}
}
\value{
a list of five tibbles; the first four representing the four overlays --
the data, the common, the condition, and the residuals -- and a fifth tibble
showing the source table of the analysis.
}
\description{
Splitting your data to construct overlays that mimic the four sections of
data used to calculate a one-way ANOVA.
}
\examples{
\dontrun{
# using dplyr to see the data split by condition, assuming a mean common
ToothGrowth \%>\% group_by(supp) \%>\% summarize(mn = mean(len))

# using ANOVA_overlay
ANOVA_overlay("supp", "len", ToothGrowth)
ANOVA_overlay("supp", "len", ToothGrowth, common = median)
ANOVA_overlay("dose", "len", ToothGrowth)

# using ANOVA_overlay with formula
ANOVA_overlay(len~supp, ToothGrowth)

# Showing that the sums of the overlays add up to the data

toothGrowthOverlays <- ANOVA_overlay("supp", "len", ToothGrowth)
SS_common <- toothGrowthOverlays$overlays$common \%>\% .^2 \%>\% sum()
SS_condition <- toothGrowthOverlays$overlays$condition \%>\% .^2 \%>\% sum()
SS_residuals <- toothGrowthOverlays$overlays$residuals \%>\% .^2 \%>\% sum()
SS_data <- toothGrowthOverlays$overlays$data \%>\% .^2 \%>\% sum()
SS_overlays <- SS_common + SS_condition + SS_residuals
all.equal(SS_data, SS_overlays)
}
}
