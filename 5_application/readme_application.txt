This procedure is the same for the 3 models. They all start with combining
the datasets on unit level. For privacy reasons, this is not published. 
Therefore, all models start with the R script "2lcs.R". In this script 
LC models are created for the 5 bootstrap samples in the combined dataset.

In the script "3impute.R" the posterior membership probabilities are obtained
from the 5 LC models, and are used to impute 5 new variables. Also, the short
dataset is turned into a  dataset in long format. 

in "4_pool_burgstat.R" and "4_pool_toeslag.R", the estimates of interest are
obtained and are pooled using Rubins rules. 