
d is a spatial data frame, created by:

```r
coordinates(d) <- ~long+ lat
```

as long as it has fields for lat and long

```r
kn <- knearneigh(d, k=4, longlat =TRUE) # 4 nearest neighbours may not be appropriate - lots of other options
nb <- knn2nb(kn)
lw <- nb2listw(nb)

e1 <- biomass ~ I(MAP/10) + I(MinT/100) +
                CLY + CEC  + Elev +
                Fire + Pop + I(Travel/60) + I(Herb/1000)

m1 <- lm(e1, data = d); summary(m1)
lm.morantest(m1, lw)
```

if Moran's I is significant, then test which model might be better:

see: http://labs.bio.unc.edu/buckley/documents/anselinintrospatregres.pdf [labs.bio.unc.edu]

```r
b.lagrange <- lm.LMtests(m1,lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
```

Try a SAR model. "LU" provides an alternative sparse matrix decomposition approach. In addition, there are "Chebyshev" and Monte Carlo "MC" approximate log-determinant methods.

```r
b.err <- errorsarlm(e1, data = d, listw = lw, method = "LU")

summary(b.err, Nagelkerke = TRUE, Hausman = TRUE)
qqnorm(residuals.sarlm(b.err))
hist(residuals.sarlm(b.err))
```
