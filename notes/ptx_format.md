# Notes on the format of ptx files

[PTX format - Matterform Scanner](https://sites.google.com/site/matterformscanner/learning-references/ptx-format)

Annotated example of PTX file:

```
20224  # Number of columns
8615  # Number of rows
482595.121831 8330769.987967 1254.138086  # Scanner registered position (xyz)
-0.990870 -0.134818 -0.000312  # Scanner registered axis 'X'
0.134818 -0.990870 -0.000175  # Scanner registered axis 'Y'
-0.000285 -0.000215 1.000000  # Scanner registered axis 'Z'
-0.990870 -0.134818 -0.000312 0  # Tranformation matrix, rotation and translation 4x4 matrix
0.134818 -0.990870 -0.000175 0 
-0.000285 -0.000215 1.000000 0
482595.121831 8330769.987967 1254.138086 1
0 0 0 0.500000  # Start of point coordinates
0 0 0 0.500000  # Unreturned pulses
0 0 0 0.500000  
0 0 0 0.500000
-0.000046 0.909775 -1.885635 0.010376  # First returned pulse
-0.000046 0.903366 -1.870834 0.015015
-0.000046 0.895859 -1.853836 0.019165
-0.000046 0.894424 -1.849380 0.020874
-0.000046 0.898849 -1.857010 0.024781
```

.ptx files can contain multiple scans, each separated by the header material.

Note that line 3 and the line 10 are identical, as are lines 4-6 and 7-9. This means top 6 lines can be discarded.

Coordinate unit specified by exporter, in my case Cyclone using UTM zone.

Apply transformation matrix to convert points to projected coordinate system. This is needed to line up the scans when combining them. First, split the file into individual scans based on the rows containing the dimensions.

