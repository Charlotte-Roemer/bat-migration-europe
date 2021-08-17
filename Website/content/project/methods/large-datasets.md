---
background: secondary
fragment: content
title: Working with very large datasets
title_align: left
weight: 150
---


In this project, a very large dataset of acoustic recordings will be analysed. This context makes it possible to work with some noise in the data:

Variability appears in data due to the type of measure or analysis: this is called **noise**.
- e.g. Error in species ID

Noise is not always a problem: the law of large numbers says that noise can always be compensated by the quantity of data collected.

**Bias** appear in results when the noise is correlated with the studied variables.
- e.g. If the Common Noctule is more often mistaken with the Serotine bat in forests compared to meadows 
--> The study of the influence of the habitat on the activity *could* lead to biased results.

**However, if the correlation is small enough, then this bias is insignificant! If this correlation can be measured, and if it is not too strong, then this bias can be corrected through modelling!**

--> With big datasets, it is important to perform the analysis with different thresholds of error probabilities in the automatic ID, to assess the robustness of results ([Barré et al. 2019](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13198)).
