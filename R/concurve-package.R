#' @title A description of the concurve R package
#' @name concurve-package
#' @aliases concurve
#' @docType package
#' @author Zad Rafi, Andrew D. Vigotsky, and Aaron Caldwell
#' @description
#' Allows one to compute compatibility (confidence)
#' intervals for various statistical tests along with their corresponding
#' P-values, S-values, and likelihoods. The intervals can be plotted to
#' create consonance, surprisal, and likelihood functions allowing one to
#' see what effect sizes are compatible with the test model at various
#' compatibility levels rather than being limited to one interval estimate
#' such as 95%.
#' \tabular{ll}{ Package: \tab concurve\cr
#'  Logo: \tab \figure{logo.png}{options: width="50px"}\cr
#'   Type: \tab Package\cr Version: \tab
#' 2.7.5\cr Date: \tab 2020-08-09\cr License: \tab GLP-3}
#' @details
#' Accepts most modeling functions that produce confidence intervals to construct distributions.
#' Instructions are also provided on how to construct similar graphs for [Stata](https://data.lesslikely.com/concurve/articles/stata.html).
#' ##### See the following articles:
#'  * **[Comparison to Bayesian Posterior Distributions](https://data.lesslikely.com/concurve/articles/bayes.html)**
#'  * **[The Bootstrap and Consonance Functions](https://data.lesslikely.com/concurve/articles/bootstrap.html)**
#'  * **[Background Literature](https://data.lesslikely.com/concurve/articles/literature.html)**
#'  * **[Customizing Plots](https://data.lesslikely.com/concurve/articles/customizing.html)**
#'  * **[Examples in R](https://data.lesslikely.com/concurve/articles/examples.html)**
#'  * **[Profile Likelihoods](https://data.lesslikely.com/concurve/articles/likelihood.html)**
#'  * **[Meta-Analysis Examples](https://data.lesslikely.com/concurve/articles/meta-analysis.html)**
#'  * **[Using Stata](https://data.lesslikely.com/concurve/articles/stata.html)**
#'  * **[Survival Modeling](https://data.lesslikely.com/concurve/articles/survival.html)**
#'  * **[S-values](https://data.lesslikely.com/concurve/articles/svalues.html)**
#'  * **[Generating Tables](https://data.lesslikely.com/concurve/articles/tables.html)**
#'  * **[Troubleshooting](https://data.lesslikely.com/concurve/articles/troubleshooting.html)**
#'  * **[Consonance Functions for Linear Mixed-Effects Models](https://data.lesslikely.com/concurve/articles/variancecomponents.html)**
#'  * **[Wish List](https://data.lesslikely.com/concurve/articles/wishlist.html)**
#' @references Rafi, Z., and Greenland, S. (2020),
#' “Semantic and Cognitive Tools to Aid Statistical Science:
#' Replace Confidence and Significance by Compatibility and Surprise"
#' BMC Medical Research Methodology
#' <https://arxiv.org/abs/1909.08579>
#' @references Fraser DAS. The P-value function and statistical inference. The American Statistician. 2019;73(sup1):135-147. doi:10.1080/00031305.2018.1556735
#' <https://doi.org/10.1080/00031305.2018.1556735>
#' @references Fraser DAS. P-Values: The Insight to Modern Statistical Inference. Annual Review of Statistics and Its Application. 2017;4(1):1-14.
#' <https://doi.org/10.1146/annurev-statistics-060116-054139>
#' @references Poole C. Beyond the confidence interval. American Journal of Public Health. 1987;77(2):195-199. doi:10.2105/AJPH.77.2.195
#' <https://doi.org/10.1002/jrsm.1410>
#' @references Poole C. Confidence intervals exclude nothing. American Journal of Public Health. 1987;77(4):492-493. doi:10.2105/ajph.77.4.492
#' <https://doi.org/10.2105/ajph.77.4.492>
#' @references Schweder T, Hjort NL. Confidence and Likelihood*. Scandinavian Journal of Statistics. 2002;29(2):309-332. doi:10.1111/1467-9469.00285
#' <https://doi.org/10.1111/1467-9469.00285>
#' @references Schweder T, Hjort NL. Confidence, Likelihood, Probability: Statistical Inference with Confidence Distributions. Cambridge University Press; 2016.
#' <https://books.google.com/books/about/Confidence_Likelihood_Probability.html?id=t7KzCwAAQBAJ>
#' @references Singh K, Xie M, Strawderman WE. Confidence distribution (CD) – distribution estimator of a parameter. arXiv. August 2007.
#' <http://arxiv.org/abs/0708.0976>
#' @references Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42. doi:10.1097/00001648-199001000-00009
#' <https://doi.org/10.1097/00001648-199001000-00009>
#' @references Whitehead J. The case for frequentism in clinical trials. Statistics in Medicine. 1993;12(15-16):1405-1413. doi:10.1002/sim.4780121506
#' <https://doi.org/10.1002/sim.4780121506>
#' @references Xie M-g, Singh K. Confidence Distribution, the Frequentist Distribution Estimator of a Parameter: A Review. International Statistical Review. 2013;81(1):3-39. doi:10.1111/insr.12000
#' <https://doi.org/10.1111/insr.12000>
#' @references Rothman KJ, Greenland S, Lash TL. Precision and statistics in epidemiologic studies. In: Rothman KJ, Greenland S, Lash TL, eds. Modern Epidemiology. 3rd ed. Lippincott Williams & Wilkins; 2008:148-167.
#' @references Rücker G, Schwarzer G. Beyond the forest plot: The drapery plot. Research Synthesis Methods. April 2020. doi:10.1002/jrsm.1410
#' <https://doi.org/10.1002/jrsm.1410>
#' @references Cox DR. Discussion. International Statistical Review. 2013;81(1):40-41. doi:10/gg9s2f
#' <https://onlinelibrary.wiley.com/doi/abs/10.1111/insr.12007>
#' @seealso
#' \code{\link{curve_gen}},
#' \code{\link{ggcurve}},
#' \code{\link{curve_table}}
#'
NULL
