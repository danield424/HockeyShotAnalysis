# Starter folder

## Overview

This repo provides students with a foundation for their own projects associated with *Telling Stories with Data*. You do not need every aspect for every paper and you should delete aspects that you do not need.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from X.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the code were written with the help of the auto-complete tool, Codriver. The abstract and introduction were written with the help of ChatHorse and the entire chat history is available in inputs/llms/usage.txt.

## Some checks

- [ ] Change the rproj file name so that it's not starter_folder.Rproj
- [ ] Change the README title so that it's not Starter folder
- [ ] Remove files that you're not using
- [ ] Update comments in R scripts
- [ ] Remove this checklist




# notes on methodology
updated project outline:
1. explore/analyze gsaa for goaltenders:
- overall gsaa over all shots - see if this is close to 0 (as it should be)
- gsaa for individual goaltenders to see top/worst performers

2: explore shot types and shot locations
- see if overall gsaa is close to 0 for all shot types, shot locations. 
consider adding comparisons for high-danger vs. low-danger areas, as well as shot types

3. build 'game state'
- analyze score (tied, trailing, leading, game score difference within 1, or greater)
- analyze strength situations (5v4/6v5, 4v5, 5v5/4v4/3v3)
- reg season vs playoffs
- analyze gsaa thru this lens

4. build 'recent game performance'
- for shots look at rolling shot density over 5 min intervals
- as well: number of shots faced divided by time elapsed. (shot frequency so far during game)
- analyze gsaa thru this lens
4.5. look at gsax per shot. to see if facing difficult shots suddenly = goal. consider this for an interaction term in final model

5. build 'past games performance'
- look at gsaa, actual goals allowed, wins in last game/last few games
- Include streakiness or variance in GSAA/game performance as an additional factor.
- analyze gsaa thru this lens.

6. build predictive model
Plan to evaluate the predictive model using metrics like log loss (for probabilities) or calibration plots, alongside traditional accuracy metrics.
not sure how this will work yet - but the idea is to predict if a goaltender will save a shot given xgoal, and all the other mentioned factors - basically an 'upgraded' xgoal
evaluate the model not just by accuracy but also by its ability to highlight key factors influencing shot saves.



refs
r, tidyverse
https://moneypuck.com/data.htm 
https://www.hockey-reference.com/awards/voting-2024.html vezina votes
https://www.expectedbuffalo.com/chaos-unmasked-part-2-hypothetical-voodoo-nhl-goaltenders/ 
https://edge.nhl.com/en/glossary#m high danger shots
https://dobberhockey.com/2024/07/29/the-wild-west-high-danger-shooters/ 
https://media.nhl.com/site/asset/public/ext/2021-22/2021-22Rules.pdf faceoff dots

