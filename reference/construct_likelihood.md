# Construct Likelihood Function for Statistical Models

Build likelihood, log-likelihood, and deviance functions from scratch
without external dependencies. Supports profile likelihood and
likelihood-based inference.

## Usage

``` r
construct_likelihood(model = NULL, data = NULL, formula = NULL,
  family = gaussian(), method = c("auto", "numeric", "analytic"))
```

## Arguments

- model:

  Fitted model object (lm, glm, etc.) or NULL to build from scratch

- data:

  Data frame (required if model is NULL)

- formula:

  Model formula (required if model is NULL)

- family:

  Family object for GLMs (default: gaussian)

- method:

  Method for likelihood construction: "auto", "numeric", "analytic"

## Value

List containing likelihood functions and methods

## Details

Based on:

- Pawitan (2001). In All Likelihood. Oxford University Press.

- Venzon & Moolgavkar (1988). A method for computing
  profile-likelihood-based confidence intervals. Applied Statistics,
  37(1), 87-94.

- Murphy & van der Vaart (2000). On profile likelihood. JASA, 95(450),
  449-465.
