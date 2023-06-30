A series of jupyter notebooks covering my project for Erdos Institute's 2023 Data Science Bootcamp.

I started and led this team project, which aims to explore and exploit cross-crop systemic losses to better inform agricultural portfolio management and crop insurance rating. 
While the full project included other members of the bootcamp, these codes are all of my own creation.

The order of running:
1. SoB_Assembly : Assembles raw dataset from USDA Summary of Business files
2. CommoditySelection: There are too many commodities and policies to analyze in the entireity of the USDA dataset. This code selects a short-list based on the available overlap between the members of the short-list within same county-years as well as selecting for major commodities with a high total liabilities in the system.
3. SummaryStatistics_BaselineModel: Calculates summary statistics for the shortlist and generates MSE for a baseline model. The baseline is the running average of the loss ratio for that county-commodity
4. LASSO : Conducts cross-validated LASSO analysis, where logged loss ratio for each coverage level X commodity is predicted against the same for each other coverage level X commodity
5. Results_Analysis : Collects and reorganizes results from LASSO in preparation for presentation

The final results found relatively little. While I would like to return to this idea with new ideas and time, it also stands as a lesson to invest more in EDA ahead of model training.
