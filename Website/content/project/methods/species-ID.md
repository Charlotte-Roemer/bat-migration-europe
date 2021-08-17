---
background: light
fragment: content
title: Species ID
title_align: left
weight: 140
---

Human identification of bat echolocation calls can be of very good quality but it also brings observer bias (i.e. two observers might not agree on the identity of the same sound sequence). The quantity of data and observers in this project makes it impossible to control observer bias. This is why we chose to use automatic ID. 

Although not perfect, automatic ID makes it possible to assess the error rate, which can be used to sort out data (e.g. all sound sequences with an error probability superior to 0.1 will not be used) or to associate data with weights (e.g. sound sequences with a higher error probability will be assigned a lower weight). Moreover, automatic ID allows the re-analysis of the whole dataset in a reproducible way in the future, using this process:

- Extract sound parameters with TADARIDA ([Bas et al., 2017, open source](https://openresearchsoftware.metajnl.com/articles/10.5334/jors.154/)) and make ID predictions
- The classifiers proposes ID along with an error probability
- Account for uncertainty in automatic ID by assessing the robustness of results for different confidence indexes (see [Barré et al. 2019](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13198))

