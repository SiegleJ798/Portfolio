These R files conduct a long series of regressions investigating if a drought in the previous year worsens crop losses from another drought in the current year, controlling for the actual severity of the drought.

Order of running:
1. Dataframes_DroughtEssay : Organizes and cleans the several datafiles
2. DroughtMonitorAnalysis : Conducts several variations of the regression format, logit for >0 and beta regression for % of acres lost, for corn and soybeans.

The results indicate that successive droughts have a detrimental effect even after controlling for drought severity. Furthermore, they drastically reduce the protective power of irrigation.
However, in hindsight these R files are not well organized. Instead of creating such a large number of text files, I should have automated stripping key performance indicators from the fitted models, and then created a smaller set of files summarizing the best working regression formats.
