# The Bootstrap and Consonance Functions

Some authors have shown that the bootstrap distribution is equal to the
confidence distribution because it meets the definition of a consonance
distribution.^([1](#ref-efronFisher21stCentury1998)–[3](#ref-xieConfidenceDistributionFrequentist2013))
The bootstrap distribution and the asymptotic consonance distribution
would be defined as:

\\H\_{n}(\theta)=1-P\left(\hat{\theta}-\hat{\theta}^{\*} \leq
\hat{\theta}-\theta \| \mathbf{x}\right)=P\left(\hat{\theta}^{\*} \leq
\theta \| \mathbf{x}\right)\\

Certain bootstrap methods such as the `BCa` method and `t`-bootstrap
method also yield second order accuracy of consonance distributions.

\\H\_{n}(\theta)=1-P\left(\frac{\hat{\theta}^{\*}-\hat{\theta}}{\widehat{S
E}^{\*}\left(\hat{\theta}^{\*}\right)} \leq
\frac{\hat{\theta}-\theta}{\widehat{S E}(\hat{\theta})} \|
\mathbf{x}\right)\\

Here, I demonstrate how to use these particular bootstrap methods to
arrive at consonance curves and densities.

We’ll use the `Iris` dataset and construct a function that’ll yield a
parameter of interest.

### The Nonparametric Bootstrap

We can now use the [`curve_boot()`](reference/curve_boot.md) method to
construct a function. The default method used for this function is the
“`Bca`” method provided by the
[`bcaboot`](https://cran.r-project.org/package=bcaboot)
package.^([**efron2018?**](#ref-efron2018))

I will suppress the output of the function because it is unnecessarily
long. But we’ve placed all the estimates into a list object called y.

The first item in the list will be the consonance distribution
constructed by typical means, while the third item will be the bootstrap
approximation to the consonance distribution.

We can also print out a table for TeX documents

More bootstrap replications will lead to a smoother function. But for
now, we can compare these two functions to see how similar they are.

If we wanted to look at the bootstrap standard errors, we could do so by
loading the fifth item in the list

where in the top row, `theta` is the point estimate, and `sdboot` is the
bootstrap estimate of the standard error, `sdjack` is the jacknife
estimate of the standard error. `z0` is the bias correction value and
`a` is the acceleration constant.

The values in the second row are essentially the internal standard
errors of the estimates in the top row.

------------------------------------------------------------------------

The densities can also be calculated accurately using the `t`-bootstrap
method. Here we use a different dataset to show this

Our function is a simple mean difference. This time, we’ll set the
method to “`t`” for the `t`-bootstrap method

The consonance curve and density are nearly identical. With more
bootstrap replications, they are very likely to converge.

### The Parametric Bootstrap

For the examples above, we mainly used nonparametric bootstrap methods.
Here I show an example using the parametric `Bca` bootstrap and the
results it yields.

First, we’ll load our data again and set our function.

Now, we’ll use the same function, but set the method to “`bcapar`” for
the parametric method.

Now we can look at our outputs.

We can compare the functions to see how well the bootstrap
approximations match up

That concludes our demonstration of the bootstrap method to approximate
consonance functions.

## Cite R Packages

Please remember to cite the packages that you use.

------------------------------------------------------------------------

## References

------------------------------------------------------------------------

1\.

Efron B. R. A. Fisher in the 21st century (Invited paper presented at
the 1996 R. A. Fisher Lecture). *Statistical Science*.
1998;13(2):95-122. doi:[10/cxg354](https://doi.org/10/cxg354)

2\.

Efron B, Narasimhan B. The automatic construction of bootstrap
confidence intervals. October 2018:17.

3\.

Xie M, Singh K. Confidence Distribution, the Frequentist Distribution
Estimator of a Parameter: A Review. *International Statistical Review*.
2013;81(1):3-39.
doi:[10.1111/insr.12000](https://doi.org/10.1111/insr.12000)
