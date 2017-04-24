Open the script in "1_TOP_executesim.R". From this script the simulation is
executed. This is done by running the prep_sim function in "2_preparesim.R".

In the prep_sim function, the dataset corresponding to the simulation condition
is loaded from the createdata folder. Also, the corresponding logit coefficients
for the brew file are created. Next, the simulation function from the file
"2_simulate.R" is run. 

The simulation function starts with separating the large sample file in a 
separate file for each sample, and by creating "nboot" bootstrap samples for
each sample. (nboot=m, default is 5). Each sample file including bootstrap
samples is saved as a short dataset in a separate file. 

Then for each iteration in the simulation study, a sample is loaded. On each
bootstrap sample within a sample, a latent class model is fitted by using the
"2_runtemplate.R" function, and the "brew_model.brew" file. The correct logit
coefficients were inserted in the brew file in the prep_sim function. For samples
where the marginal of the "impossible combination" of covariate Z is very small,
it can be the case that a sample is drawn where this is 0. In that case, an 
error occurs because we assign a starting value to a parameter that is actually
not there. If this happens, use "brew_model2.brew". 

Next, the conditional probabilities for each indicator in each latent class
are computed and a profile is created comparable to the profile in Latent Gold,
by using the functions conditional.probabilities and conditional.probabilities.2
which are both in "2_conditional.R". 

In the next step, the function posterior.probabilities in "2_posterior.R" is 
used to calculate the posterior probabilities for the LC models based on the 
bootstrap samples. 

With the impute.samples function in "2_impute.R" we go back to the original
short sample, where we add the posterior probabilities of the m bootstrap samples
and create a dataset in long format. Next, we create m empty variables
which we impute by drawing from the m posterior probabilities. 

The prep_sim function returns an .Rdata file containing for every sample the 
conditional probabilities, the posterior probabilities and the imputations.


Now, we open the script "1_TOP_evaluate.R". This scripts obtains the relevant
population values which are used to estimate bias etc. in the evaluation
function from "3_evaluation.R". In this function we start by obtaining for both
the 2x2 table of W and Z and the logistic regression of W on Q all the relevant
results per imputation and per simulation. Next, we average these results over the
simulations, and we pool the multiple imputations. At last, a list containing
the results per simulation and the pooled results are returned. In case you used
"brew_model2.brew", you should also use "3_evaluation2.R". 