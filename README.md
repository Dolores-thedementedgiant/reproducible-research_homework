# Reproducible research: version control and R

## Q1-Q3 see link below:

-- [ Logistic growth repo ](https://github.com/Dolores-thedementedgiant/logistic_growth)

## Q4: (30 points) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

-- A script for simulating a random_walk is provided in the question-4-code folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)

Even if two random walks both start from the same position which is the origin (0,0), they can end up in very different positions after the same number of steps just out of randomness.
These simulations lack reproducibility: Different results are generated each time when the code is re-executed.
Positions tend to disperse in the available space during random walks.
The next step in a random walk does not depend on the past step, it only depends on the current position.


-- Investigate the term random seeds. What is a random seed and how does it work? (5 points)

Random seed is a value input given to the pseudorandom number generator algorithm at the start of simulation that initializes the random walk. The algorithm then produces a sequence of numbers that are seemingly random but are actually based on the initial random seed value. The same random seed gives rise to identical simulations if number of steps remains constant. By setting random seeds, random walk simulations can be reproduced because the algorithm will produce the same sequence of numbers each time the code is executed. This can be useful when random walks are applied to reproducible simulations such as Brownian motion. 

-- Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked reproducible-research_homework repo. (10 points)

 [Code for reproducible simulation of Brownian motion](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/blob/main/question-4-code/random_walk.R)

-- Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the README.md of the fork). (5 points)

Click this link: ![Commit Changes](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/commit/ce9d0083c5d3a23ec66b17757dbf2bd6f8ef814c) 

Or look at this incomplete image: 
![Screen Shot 2023-12-05 at 08 57 11](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/assets/148791070/683a4234-fa11-4678-adef-4e1e369128b4)

## Q5: (30 points) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

-- Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the question-5-data folder). How many rows and columns does the table have? (3 points)

The table has 33 rows and 13 columns

-- What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)

Log transformations can be used to fit a linear model to this data.

-- Find the exponent (alpha) and scaling factor (beta) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant?Compare the values you found to those shown in Table 2 of the paper, did you find the same values? (10 points)

The exponent alpha = 1.52 and scaling factor beta = 1182. See below for how they are obtained.

![Solving for beta and alpha](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/assets/148791070/6777f351-fa58-45f9-a879-d09d20ca0ef3)

p-value for slope = 6.44e-10

p-value for intercept = 2.28e-10

They are statistically significant because both p-values are smaller than 0.05.

The values of alpha and beta in the paper are 1.52 and 1182 respectively. They are identical to the values I obtained from my model output.

[Code of linear model](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/blob/main/Question%205%20model%20and%20plot%20code.R)

-- Write the code to reproduce the figure shown below. (10 points)

[Code to reproduce this figure](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/blob/main/Question%205%20model%20and%20plot%20code.R)

The image from my code is attached below:

![Linear regression virus](https://github.com/Dolores-thedementedgiant/reproducible-research_homework/assets/148791070/a249225d-a757-4279-82a2-d5420fbe4524)

## Bonus (10 points) 
-- Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform protocols.io).

Reproducibility is the ability to reproduce statistical results and conclusions using the same set of data as the original author of the paper while replicability is the ability to reach similar conclusions using a new set of raw data collected independently from the data used in previous researches.

Git (the version control system) and github (the web-based platform that allows code storing and sharing) enhance reproducibility by allowing others to access your code used to reach conclusions without changing the original copy owned by the author. This means that other people who want to reproduce data processing and statistical tests on their own machine can do so easily without having to email the author for code or try to write the code themselves, motivating others in the research community to verify the result. It can also prevent harmful practices such as p-hacking because the author is conscious that the code is publically available so the data analysis can be challenged any moment. 

Additionally, git and github also enhance replicability because the platform allows others to build upon the original code of the author. In this way, people can adapt the same data analysis to a new raw dataset to see if the conclusion still holds. Since github also supports collaboration, it allows physically separated researchers to access the same data and code. This is potentially useful for data collection and standardized processing in multiple locations.

However, there are several limitations and drawbacks for using github in the wider scientific community. Firstly, it has complicated design and numerous features that perplex a beginner user. Considering that it is not a standard practice in the scientific community to use github for public access of code, researchers that have not used it before might be discouraged from starting on github. Secondly, if the code from your project contains content protected by intellectual property law, the code can not be made available on a public access platform like github. Thirdly, github keeps all past history of commit changes which might make it difficult for user to look up the change they care about. Fourthly, simply using github and version control do not automatically guarantee the same environment for code execution on your machine. Extra steps like making sure packages required are installed and loaded properly are needed for code to re-run.

## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
