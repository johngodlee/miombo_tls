# Newnham et al. 2012 - Evaluation of Terrestrial Laser Scanners for Measuring Vegetation Structure

Hardware and software filtering options provided for the phase-shift scanners are sub-optimal for vegetation.

Optimal use of the Riegl VZ1000 would require a tilt mount in order to scan the entire upper hemisphere due to its limited field of view 30-130deg-zenith


Time of Flight instruments take longer than Phase shift instruments

Ni-Meister et al. 2010, Seidel et al. 2011, Yao et al. 2011 - all give examples of estimating vegetation structural metrics and the inference of above-ground biomass from TLS data

Definitions:

* Time-of-flight discrete-return scanners: emit a pulse of laser and use analogue electronics to measure the time of flight of a return echo from intercepted targets. Some machines allow detection of multiple returns from a single pulse, but there is often some 'dead time' after a return until subsequent returns can be registered, related to the outgoing pulse duration
    * High accuracy at long range and resolve gaps well
    * Pulse frequency ~ 100,000 points sec^-1

* Phase based scanners: Use a constant wave laser with intensity modulated at a series of frequencies. Shifts in phase of the returned modulations are used to determine range. Can sample at much higher frequency than time-of-flight scanners, ~millions points sec^-1
    * Normally lighter and cheaper than time-of-flight scanners
    * Shorter range ~100 m 
    * The data can be noisy around the edges of objects, because a single beam can be partly intercepted by the object edge, the return of which is range averaged, giving a fuzzy edge. This means that gaps are harder to resolve. Some software packages effectively correct this.
    * Park et al. 2010, Antonarakis 2011, and Balduzzi et al. 2011 measure canopy structure with phase shift scanners.
    * The Leica HDS7000 is a phase based scanner.

* Time-of-flight waveform scanners: record the full time trace of returned laser energy, i.e. a waveform. The data can retain the full intensity trace which can be processed when the scanner is being used in very complex media, small objects irregularly distributed. 
    * Jupp et al. 2008 uses the CSIRO ECHIDNA for vegetation canopies
    * The Riegl VZ1000 is a full waveform scanner

As well as the TLS, they also measured crown dimensions with tape measures, tree top height with a hypsometer.

Version 1.4 of the Common lidar Data Exchange Format can maintain the association between multiple returns and a single pulse zenith and azimuth, as in the Riegl VZ1000. But this data doesn't normally contain information for pulses with no return (i.e. canopy gaps), small fluctuations of rotation speed and laser pulse rate can result in an irregular scan pattern, preventing the data being sorted into scan order, and the origin of the sensor coordinate system differs from the laser pulse origin, particularly problematic at near range. SO, they made some freely available C++ code that processes the raw binaries.

Can generally just add the height of the instrument optical centre to the vertical (Z) coordinate in the data. You can also derive a DEM from a scan, in this example using an adaptation of the method in Zhang et al. (2003).

Deviation of the distance of returns from the scanner from an exponential decay due to obscuring by material are due to the clumping (i.e. non-random) of vegetation components into discrete trees.

The Riegl VZ1000 had a greater magnitude of returns for the foliage profiles, they also had greater penetration into the upper reaches of the canopy.

Default filtering for the Leica HDS7000 is not adequate to distinguish non-interceptions from partial beam interceiptions. Without correction this would over-inflate estimates of gap fraction.



