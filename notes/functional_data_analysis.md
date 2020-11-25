---
title: "Functional data analysis"
...


Functional data analysis is concerned with analysing curves, surfaces, or other things that may vary over a continuum.

Functional data analysis assumes that curves are intrinsically smoothed, accounting for measurement error in observations, or stochastic variablity.

There are many branches of functional data analysis which overlap with basic multivariate statistical analysis. For example, linear models and Principal Component Analysis (PCA).

Febrero-Bande and de la Fuente 2012 - Statistical computing in functional data analysis: The R package fda.usc

[CRAN info page on functional data analysis](https://cran.r-project.org/web/views/FunctionalData.html)

A basis function is a mathematical expression made of building blocks into a linear additive function to describe a curve of unknown complexity. Polynomials are examples of simple basis functions, but these have been superceded by spline functions, which are more flexible.

Most data-fitting now uses Fourier series for periodic or near periodic data such as weather data. When the data are not periodic we can use splines. Splines are more computationally intensive than polynomials, but the difference is negligible with modern computing. Polynomials, Fourier series and wavelets can only be used to calculate coefficients in specialized situations, for example, when `t` is equally spaced. 

Gorecki and Smaga 2019 - fdANOVA: an R software package for analysis of variance for univariate and multivariate functional data

Functional data is peculiar in that the number of variables (time points for example) far outweighs the number of observations (often one per time point).

For fdAnova, data should be a continous grid of time points. When the time points are not equally spaced, follow the methodology of Zhang (2013). Use `stats::smooth.spline()` or functions from the `{splines}`, `{bigsplines}`, or `{pspline}` packages


