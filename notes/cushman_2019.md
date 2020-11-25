% Cushman and Kellner 2019 - Prediction of forest aboveground net primary production from high-resolution vertical leaf-area profiles

Temperature and precipitation explain ~50% of variation in aboveground net primary productivity (ANPP) in tropical forest sites, but determinants of remaining variation are poorly understood.

Tested whether leaf area and vertical arrangement can predict ANPP when other variables are held constant.

Used airborne LiDAR in neotropical rain forest.

Vertical leaf area profiles predict 38% of remaining (?) variation among plots in ANPP. This is 4.5x the variance explained when using total leaf area and 2.1 times that using canopy height.

ANPP increases under warmer and wetter conditions (Cleveland et al. 2011, Taylor et al. 2017).

> This knowledge gap limits our ability to develop a biolgoical understanding of carbon and water fluxes at ecosystem and larger scales (Schimel et al. 2014, Clark et al. 2017)

Why would leaf area distribution predict ANPP?

* Measurements of the net exchange of CO2 using eddy covariance indicate that the quantity of absorbed photosynthetically active radiation predicts net ecosystem exchange and that some forests are light limited. Total leaf area and how it is arranged spatially partly determines light absorption.
* Leaf nutrient concentration per unit area, area-based carbon fluxes and leaf mass per unit area change predictably with height and respond more strongly to height than to light environments in tropical forests (Meir et al. 2001, Cavaleri et al. 2008, 2010). i.e. the efficiency with which absorbed light drives phoyosynthesis may depend on the 3D arrangement of leaves
* Vertical leaf area distribution is correlated with life-history variation and changes in forest composition during succession (Becknell et al. 2018, Stark et al. 2015).

There have been previous studies looking at total leaf area and its relationship with productivity (Waring 1983; Kitayama & Aiba 2002; Doughty & Goulden 2008).

They used Discrete Relative Height (DRH) percentiles as an alternative to leaf-area vertical profiles to quantify leaf area vertical distribution. By comparing DRH to leaf-area vertical profiles they can test that the vertical distribution of _leaf area_, as opposed to the vertical distribution of _point height measurements from LiDAR_ improves the predictive power of canopy structure.

DRH percentiles are the height below which a given percentage of lidar point measurements were recorded.

Their LiDAR yields a density of 3 returns per m^2 and a maximum of 2 returns per laser pulse.

Used the algorithm from Detto et al. (2015) to quantify vertical leaf-area profiles from discrete return lidar data. This model describes the amount of light `I` that reaches depth `z` in direction `O` as a function of the radiation at the top of the canopy in direction `O` minus the light intercepted before height `z`. This allows them to calculate lead area over 1m vertical bins averaged over the 50 m x 100 m horizontal extent of the plot. Assumes a spherical distribution of leaf angles (Detto et al. 2015).

__Statistical analyses:__

Predicted ANPP from vertical leaf-area profiles using a partial least squares regression model. Like a PCA this method calculate a linear transformation of input variables (e.g. vertical leaf-area profiles) into orthogonal component variables. Unlike PCA, where input data are transformed to maximise the variance of newly transformed component variables, Partial least squares transforms to maximise the covariance of trnasformed component variables and a response variable (Mevik and Wehrens 2007). 

Revisit again for statistical hypothesis testing.

> These results highlight the importance of biotic factors in driving ANPP because leaf-area profiles are influenced by species composition (Asner et al. 2008) and succession within a single landscape where precipitation and temperature are invariant (Kellner et al. 2011, Becknell et al. 2018).

Soil fertility is also an important determinant of ANPP that can explain 7-18% of ANPP not explained by MAP and MAT. 

Litter production is predicted better by leaf area vertical distribution than wood production.

Other work indicated that seasonal variation in ANPP is influenced by canopy structure (Morton et al. 2016, Cavaleri et al. 20167, Wu et al. 2017b).

> Further characterising the relationship between forest structure and ANPP at a globally representative sample of sites will permit investigation of regional drivers of ANPP using terrestrial and airborne lidar, in addition to future spaceborne lidar missions (Dubayah et al. 2014).








