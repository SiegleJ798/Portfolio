In my final thesis essay, I applied my econometric skills to machine learning concepts, and these R files include my first attempts at true cross validation.
The goal of this project was to generate a model able to price a new specialty crop insurance offering more accurately than the USDA's historical pricing methods.
The model therefore only uses historical information on local climate and the historical performacne of crop insurance offerings for other commodities.

Order of running:
1. DataCleaning : Creates several cleaned datasets and graphics. Selects potatoes as an opportune specialty crop due to a wide geographic spread and a list of overlapping commodities to serve as local indicators.
2. AllLaggedVariables : Trains the model and tests it agains the USDA's average performance over the test set.  

This project was a success, if only due to the atrotious bias in USDA crop insurance pricing. In redoing this project, I would consider adding further functionality features post-analysis, as well as investigating WHERE and WHEN the model went wrong to diagnose the remaining errors.