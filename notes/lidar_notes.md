# Notes from meeting with Steve Hancock - LiDAR, and reading

## Software packages 

[PyLiDAR](http://www.pylidar.org/en/latest/) - [Github repository](https://github.com/Ffisegydd/pylidar)

[TreeSeg](https://github.com/apburt/treeseg) - Extracting individual trees from TLS data - Also Balsi 2018, Lindberg 2017.

@@ My goals

I want to measure:

* Canopy gap fraction - a measure of light extinction at ground level
* Tree canopy vertical complexity - to see whether species diversity promotes more complex canopies 
    * What do I mean by this?
        * Canopy foliage density profiles 
* Understorey biomass - Response variable, proxy for fire risk

I want to get more accurate and time efficient measurements of canopy complexity in my woodlands. Measuring canopy areas and canopy heights with tape measures and the laser range finder is not very accurate or efficient, so I'm keen to change this.

@@ Conversation with Steve Hancock

Foliage Height Diversity. 

![](img/fhd_schem.jpg)

My initial idea was to identify peaks and troughs in the foliage height diversity profile and use those as a metric of canopy complexity. However, this is very subjective. Instead, why not just look at the whole curve and see how they differ statistically under different species compositions. 

Cumulative height profile estimate - Measure the 95% cumulative height curve for a scan, i.e. the height at which 95% of the foliage mass is below.

It's very simple and possible to estimate canopy gap fraction using a TLS, but is it any more accurate or precise than estimating from hemispherical photos? For gap fraction the benefit of the TLS comes from its efficiency, but for LAI it's much more accurate and precise than the hemi-photo.

It's possible to estimate grass height underneath the tree canopy, and from that you can calibrate with grassy biomass clipping to estimate grassy biomass. But the calibration would have to be done on each plot. Would it actually save me any time? Yes, because the scans can be integrated with the scans for the trees up above. 

If I wanted to scale these measurements up I'd be wanting to look at remote sensed Airborne LiDAR equipment (ALS). Examples:

* ICESat
* PALSAR (Phased Array type L-band Synthetic Aperture Radar)

There are more complicated methods that allow for creating hulls from points to outline tree trunks, canopies etc.. These generally require multiple scans and lots of laborious pointing and clicking to line up the points in a 3D space - See papers with Cald(v?)ers - QSM (Quantitative Structure Modelling). They might be useful when looking at crown complexity and how that affects woody biomass. 

@@ Hashimoto et al. 2004 - Estimating forest structure indices for evaluation of forest bird habitats by an airborne laser scanner

Different metrics:

* Foliage height diversity calculated by five layers (FHD5)
* FHD4 - 4 layers
* Sum of vegetation coverage in five layers (COVSUM5)
* COVSUM4 - 4 layers
* Crown patchiness

FHD was originally suggested by MacArthur & MacArthur 1961 - Calculated as:

$$ FHD = -\sum{p_i log_e p_i}$$

Where pi is the proportion of horizontal vegetation coverage in the ith layer.


@@ Jupp et al. 2009 - Estimating forest LAI profiles and structural parameters using a ground-based laser called "Echidna"

The accuracy of LAI measurements with available methods is questionable.

* Canopy photography
* Densitometer, disco ball or otherwise

This paper essentially demonstrates a LiDAR method which produces a "hemispherical photograph with range".

Key to this method is that it doesn't rely on particular weather conditions to do it.

The relationship between LAI and biomass (which depends on growth form and structure of the forest) has been used to infer biomass from remote sensed data.

Hemispherical photographic estimates of LAI have mostly assumed a random and convex leaf (thin leaves are flat, needles are straight) canopy model. This is applicable to many crops and some grasslands, but not directly to forests.

![](img/lai_pgap.png)

None of the above methods do a good job of sampling vertical structure.

But simple equations based on gap fraction don't take into account clumping and tree shape, which results in significant bias and variance that must be taken into account. The clumping of foliage into crowns and possibly into branches within crowns means there is more gap than expected if leaves were randomly distributed, which is what is assumed.

The Echidna data allow trunk effects to be analysed specifically and separately from foliage effects, and they include range information out to a defined maximum range.

The Echidna data can be used to estimate:

* LAI
* LAI over height profile
* Gap fraction

At high zenith angles, i.e. near vertical, there is high variance in Pgap (proportional gap fraction), which indicates clumping.

At mid zeniths, the path lengths through the canopy are long, the laser may not emerge from the canopy at full range. 

@@ Moorthy et al. 2011 - Field characterization of olive (Olea europaea L.) tree crown architecture using terrestrial laser scanning data

Phase-based vs. Time of Flight methods of TLS:

* Phase-based - Can capture more points faster
* TOF - Have longer range, generally


ALS is limited in its application for detailed LiDAR work because of its comparitively low points/m^2 measurements. This level of detail is lacking especially in the vertical axis of tree crown measurements.

To get enough detail for braching, their scanner was set at:

* 1500 laser pulses per sec
* Minimum spot spacing between lasr pulses is 0.026 mm x distance in meters from scanner
* Laser pulse diameter at exitance is 12.7 mm, degrading at 0.17 X distance in meters from scanner
 
The ILRIS-3D (scanner make) point cloud data was processed using a cross-setional slicing algorithm that was developed and tested in lab trials by Moorthy et al. 2008.

Foliage architecture was calculated from points above a given height (i.e. the bottom of the crown) using an algorithm.

@@ Wagner et al. 2018 -  Individual tree crown delineation in a highly diverse tropical forest using very high resolution satellite images

WorldView-2 satellite imagery - Commercially available

* Rolling ball algorithm and mathematical morphological operations - To enhance crown borders to ease delineation of individual tree crown borders
* Bimodal distribution parameters estimations - To identify shaded pixels in the gaps, borders and crowns, 
* Focal statistics  - Analysis of neighbouring pixels
* Crown detection validated by comparing delineated canopies with a sample of crowns manually delineated 

They also tested the effectiveness of species classification from the crowns.

Classification accuracy varied from 30.5-96%

LiDAR can allow automatic and accurate ITC detection, mostly above 60% of the canopy trees in temperate and forest ecosystems

Tropical forests make it difficult to delineate tree crowns, because the canopy is complex, with overlapping tree canopies, very dense, no clear boundary between individuals 

Automatic tree crown methods assume tree crowns have light pixels in the middle and darker pixels at the borders (Ke and Quackenbush, 2011), but these work best on uniform coniferous trees

The method in this paper relies on border detection

Blanchard et al. (2016) said that across the tropics, DBH and crown area have a tight allometry. So that suggests that Biomass could be reliably estimated from inverse modelling of crown size of trees.

@@ Radtke and Bolstad 2001 - Laser point-quadrat sampling for estimating foliage-height profiles in broad-leaved forests

They don't use LiDAR, rather a laser range finder with a single beam, like the Nikon Forestry Pro.

Total leaf area estimates weren't accurate with the range finder.

Another method of estimating foliage distribution through the vertical canopy profile, is to float a plumb line through the canopy and count leaves touching it. But this method is still pretty laborious, and is v. difficult in tall canopies, without scaffolding.

Another method is estimating the __cumulative LAI__ at numerous vertical positions in the canopy. LAI is estimated using photography, for example. This method tends to understimate leaf area when foliage is clustered, also can only take photos under certain conditions, otherwise results are squiff.

Optical point quadrat sampling measures heights to the lowest leaves above a set of sample points on the ground (MacArthur and Horn 1969). This method assumes that above the base of the canopy, leaves are distributed in a random fashion. The method presented in this paper is an adaptation of this method, which traditionally uses telephoto lenses, 

They measured 1000 points in a 100 m^2 plot.

In the paper they use the leaf area index estimator equation in MacArthur and Horn (1969) 

![MacArthur and Horn (1969) LAI equation](img/mac_horn_eq.png)

Rumple index can be used to measure the roughness of a surface as its ratio of flat to projected surface area (see Seidl et al. 2012a,b)

@@ Zhu et al. 2018 - Improving leaf area index (LAI) estimation by correcting for clumping and woody effects using terrestrial laser scanning

In hemi-photography, woody components were always included in the photos used to estimate LAI, so really they were measuring plant area index (PAI). Also, leaf angle is assumed to be random in hemispherical photographic calculated estimates of LAI, which use the gap fraction.

Terrestrial Laser Scanning offers a way to overcome these challenges, with appropriate processing of the data.

LAI is a key vegetation structure variable determining ecosystem functioning (Beland et al. 2011).

While there are methods to discern leaf from wood in hemispherical photos, by thresholding according to colour, the photogrpahic exposure required by these methods are often at odds with the xposure required to reliably distinguish plant from sky, making them unreliable in practice.

There have been two main ways in which TLS has been used to get LAI estimates, the first uses voxelization of the point cloud (Ma et al., 2016; Zheng et al., 2016; Zhu et al., 2018) and the other uses point-based methods using data from a single scan (Danson et al. 2007, Jupp et al. 2009, Li et al. 2017).

The voxel based method assumes nothing about the leaf spatial distribution, so under-estimation caused by non-random foliage distribution are avoided (Hosoi and Omasa, 2006). But, voxel approaches are computationally expensive, also voxel size can affect results (Béland et al., 2014; Cifuentes et al., 2014; Li et al., 2017).

To convert TLS point scans to LAI must convert penetration rate of pulses through canopy (gap fraction) to LAI using the Beer-Lambert law (Nilson, 1971):

\ln(P(\theta)) = -G(\theta)LAI_e / \cos(\theta)

Where P(\theta) is the gap fraction at viewing angle \theta, G(\theta) is the fraction of the leaf area projected on a plane normal to the zenith angle \theta. G(\theta) / \cos(\theta) is the extinction coefficient (k). LAI_e is effective LAI and assumes random foliage distribution and does not account for non-photosynthetic materials included in the estimates. To convert LAI_e to LAI must correct for foliage clumping and the contribution of woody materials. TLS can assess this clumping.

Study in Bavarian Forest National Park.

At the zenith angle of 57.5 degrees. the relationship between LAI and gap fraction becomes insensitive to leaf angle distribution (Wilson 1960).

Used a RIEGL VZ-400 for the TLS measurements. LAI estimation for the TLS is:

LAI = (1-\alpha)(-\ln(P(\theta)))\cos(\theta)/(G(\theta)\Omega)

Where \alpha is the woody material to total material ratio, \theta is the laser zenith angle, P(\theta) is the canopy gap fraction in direction \theta, G(\theta)/\cos(\theta) is the extinction coefficient and \Omega is the clumping index.

A "weight all return" method which considers all returns in each pulse was used to approximate gap fraction for each zenith ring (Calders et al., 2014, 2018; Lovell et al., 2011):

P(\theta) = 1 - \frac{\sum{1 / NR}}{N_{total}}

Where NR is the number of returns detected for each pulse from the canopy and N_{total} is the total number of outgoing laser pulses.

GO BACK TO THE METHODS ON ESTIMATING EXTINCTION COEFFICIENT G(\theta)

They gridded TLS data prior to estimating leaf inclination angle, at 0.02 m.

Clumping index was estimated GO BACK TO THIS.

A Random Forest algorithm was used to separate foliage material from woody material. GO BACK TO THIS.

NOT FINISHED

@@ Wang et al. 2020 - Assessing structural changes at the forest edge using kernel density estimation

They used Airborne LiDAR data.

European temperate mixed forests.

Measured:

* Height distribution of vegetation
* Structural variance at the forest edge

Kernel density curves represented the general structural diversity of the study area.

KDE (Kernel Density Estimation) is a non-parametric method used to estimate the probability density function of a variable.

They identified three parameters which act as indicators of the KDE curves:

* Pos - the position of the parameter _w_ that has the maximum value of the KDE curve. Position of the structural feature that has the largest proportion in the study area.
* Len - the full width at half maximum value of the kernel density curves. Degree of steepness of the estimated Gaussian curves.
* Auc - the area under the curve of the KDE. Evaluates the smoothness of the curve.

@@ Staver et al. 2019 - Spatial patterning among savanna trees in high-resolution, spatiall extensive data

They used airborne LiDAR to examine tree clustering patterns. 

They show that tree cluster sizes are governed by power laws over 2-3 orders of magnitude in spatial scale and that the parameters on their distributions are invariant with respect to underlying environment.

They worked in Kruger and surveyed 10 landscapes each greater than 6000 ha each.

The tree layer in savannas is more predictable than smaller-scale plot analyses previously suggested.

Positive relationship between rainfall and tree canopy area.

% Estimating leaf angle with LiDAR

Maybe the estimates of light extinction with canopy coverage are over-estimated. In forests, leaves are assumed to be oriented at random, an assumption that is often violated. In savannas, leaves may be angled away from the sun in order to save water and prevent UV damage to the leaves. 

Is it possible to test leaf inclination using the TLS I am taking to Angola?

@@ Li et al. 2018 - Retrieving 2-D Leaf Angle Distributions for Deciduous Trees From Terrestrial Laser Scanner Data

Leaf angle distribution has an influence on the transmission of radiation within vegetation the canopy, and the distribution of PAR. 

Gap fraction theory was developed by Nilson (1971):

P(theta) = exp[-G(theta)omega*L/cos(theta)]

Where P(theta) is the probability of a ray of light penetrating a canopy without being intercepted at an incident angle theta. G(theta) is the extinction coefficient defined as the mean projection of a unit foliage area on the plane perpendicular to the view angle theta. __G(theta) can be derived from the leaf angle distribution__.

Leaf inclination: Angle between the leaf surface normal and the zenith

Azimuthal angle: Clockwise angle between the north direction and the projection of the principal axis of foliage on the horizontal plane.

Direct methods for estimating leaf angle distribution ar extremely laborious. 

The method used in this paper is based on leaf point cloud data segmentation and filtration.

They modelled 160 individual leaves and 10 trees with different leaf numbers.

R^2 = 0.98 for leaf inclunation angle model

R^2 = 0.99 for leaf azimuthal model

__Methods:__

They used a Riegl VZ-400 TLS mounted on a tripod at 1.5 m above the ground.

Collected scans from 3 locations around each tree with leaves on and leaves off. Scanning locations were in a circle with equal azimuthal interval and nearly equal scanning distance. Scans were 3-5 m away from the tree. 0.04deg beam resolution.

The leaf off scans were necessary to remove the woody component in the scans so they could focus on the leaves.

Leaf points were voxelized using the voxel-based canopy profiling method.

FINISH

@@ Balduzzi et al. 2011 - The Properties of Terrestrial Laser System Intensity for Measuring Leaf Geometries: A Case Study with Conference Pear Trees (Pyrus Communis)

A general index for describing leaf inclination is the Leaf Angle Distribution (LAD)

They talk about LAD being a function of "leaf elevation", "leaf roll" and "leaf azimuthal angle", what are these parameters?

Estimating leaves via TLS could be problematic:

* Laser shadows may favour measurement of lower canopy leaves
* Wind may move branches and leaves during the scan
* Leaves vary in reflectance
* Very high resolution scans needed with far away leaves

In this study, the scanner was places very close (2.5 m) to the leaves, scanning individual leaves under optimal conditions. This is not a field study.

Beam divergence of 0.014 deg on their scanner.






