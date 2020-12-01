# Cifuentes et al. 2014 - Effects of voxel size and sampling setup on the estimation of forest canopy gap fraction from terrestrial laser scanning data

Compared gap fraction estimates over four sampling setups.

Gap fraction is the fraction of sky not obstructed == canopy cover.

Results showed that combining nine scans produced better results in all forest stands.

Optimal voxel size varied with stature of vegetation:

* Young - 1 cm
* Intermediate - 2 cm
* Mature - 4 cm

The limitations of phase-based scanners can be overcome by placing the scanner at different locations under the canopy. 

Belgium, Oak, Beech, Oak+Birch forest plantations.

9 plots in a 10x10 m grid in each stand.

Points don't have an area or defined volume and so are unsuitable for estimating hemispherical points (Seidel et al. 2012).

Objects located closer to the instrument will be represented by a higher density of points, resulting in an imbalanced representation of the measured 3D space.

They defined voxels based on scanner range, beam divergence and beam diameter. For a given angular divergence of the beam, the greater tha range the larger the diameter of the area covered. The radius (R) of the area in metres is given as:

R = \frac{(Di * BDv + BDm)}{2}

Where Di is the distance to the scanner in metres, BDv is the beam divergence in radians and BDm is the beam diameter at the exist of the instrument in metres. 

To calculate gap fration from TLS they simulated hemispherical images from the voxelised data, using the open source ray tracing software Persistence of Vision Raytracer (POV-Ray). POV-ray represented vxels as dark-solid non-reflective cubic objects. Whitebackground and no source of light. A ray-tracing algorithm using a virtual camera in fish-eye mode was applied to create a 2D image at v-high resolution.

They used the 2D image to estimate gap fraction at each 7.8deg annulus of zenith angle between zero and 70deg. RMSE was used to compare performance between methods.

