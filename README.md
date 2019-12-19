# Dominant Admixture Model for Topic Modeling
The Dominant Admixture Model (DAM) for Topic Modeling is proposed by Blum et. al. in ["Foundations of Data Science"](https://www.microsoft.com/en-us/research/publication/foundations-of-data-science-2/). 
This git provides an R-implementation for the model, as well as an example preprocessing and analysis on the "20newsgroups" dataset.
If you want to run this analysis yourself, be sure to download the data [here](http://qwone.com/~jason/20Newsgroups/) (use the "bydate" version).

Be aware that DAM is a textbook simplification of [TSVD](https://arxiv.org/abs/1410.6991). It has some quite heavy assumptions on the data and thus often throws an error instead of returning a result. To achieve a result with it, you should definitely do some hyperparameter tuning. I'd love to hear whether you could apply the model successfully on other datasets :)
