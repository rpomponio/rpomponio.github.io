---
layout: page
title: Software
permalink: /software/
---

## [neuroHarmonize](https://github.com/rpomponio/neuroHarmonize)

<img src="/assets/ROI_volumetric_trends.jpg" alt="trends" width="300" style="float:right;border:1px solid black"/>

Harmonization tools for multi-site neuroimaging analysis.

This package, implemented in python, extends features of previous harmonization tools for imaging:

1. Support for working with NIFTI images. Implemented with the nibabel package.
2. Separate train/test datasets.
3. Specify covariates with generic nonlinear effects. Implemented using Generalized Additive Models (GAMs) from the statsmodels package.
4. Skip the empirical Bayes (EB) step of ComBat, if desired.

Harmonization of MRI, sMRI, dMRI, fMRI variables supported. Complements the work in Neuroimage by [Pomponio et al. (2019)](https://doi.org/10.1016/j.neuroimage.2019.116450).

## [neuro_lifespan_trajectories](https://github.com/rpomponio/neuro_lifespan_trajectories)

<img src="/assets/lifespan_demo.png" alt="demo" width="300" style="float:right;border:1px solid black"/>

Interactive visualization of volumetric-age trends for anatomical brain regions.




