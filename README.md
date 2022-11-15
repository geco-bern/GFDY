# Grow Fast Die Young (GFDY)

## Abstract

While enhanced tree growth over the last decades has been reported in forests across the globe, it remains unclear whether it drives persistent biomass increases of the stands, particularly in mature forests. Enhanced tree growth and stand-level biomass are often linked with a simultaneous increase in density-driven mortality and a reduction in tree longevity. Identifying empirical evidence regarding the balance between these processes is challenging due to the confounding effects of stand history, management, and environmental changes. Here, we investigate the link between growth and biomass via the shift in the negative relationship between average tree size and stand density (tree number). We find increasing stand density for a given tree size in unmanaged closed-canopy forests in Switzerland over the past six decades and a positive relationship between growth and stand density - qualitatively consistent with simulations by a mechanistic, cohort-resolving ecosystem model (LM3-PPA). Model simulations show that, in the absence of other disturbances, enhanced growth persistently increases biomass stocks despite simultaneous decreases in carbon residence time and tree longevity, independent of assumptions about the drivers of tree mortality. However, the magnitude of simulated changes critically depends on the shape of the mortality parameterizations. Our analyses reconcile reports of growth-induced reductions of tree longevity with model predictions of persistent biomass increases, and with our finding of a trend towards denser forests in response to growth - also in mature stands. 

![](https://github.com/computationales/GFDY/raw/master/manuscript/figures/fig_1.png)

## Repository use

This repository includes the scripts to analyse the links between growth and biomass.

We use observations from Swiss forests and simulations from a demography vegetation model (LM3-PPA).

The repository is organized as follows:
- data - includes the scripts for data cleaning and the clean data which is used in analysis.
- analysis - includes the scripts for data analysis.
- manuscript - includes figures and other outputs.
- R - overall functions which are recycled.
- src - not R specific functions.
