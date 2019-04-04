# Introduction

This page is dedicated to learning how to perform a cost-effectiveness data-analysis in R. The examples are focussed on the field of Health Economics and Outcomes Research but the methods are generalisable. We do not consider R to be the only valid development platform for data science, but as good a place to start as any. The learning materials used are all freely available, thanks to all the educators and developers who shared them with everyone.

The learning philosophy used throughout is to start from the basics, to learn by doing, and to apply your learnings to an example from your field right away. The example we present builds a simple Markov chain model for a health economic decision problem, but you can replace this with any example more specific to your field.

An R Study Club is currently following the g path outlined on this page, and we will update thE page as we go along. The club is private, but the learning path is freely available. Please get in touch if you want to know more or wish to contribute. 

# Learning path

1. A working knowledge is assumed that allows you to understand what's going on in the solution of exercise 2.5 from [Decision Modelling in Health Economic Evaluation](https://www.herc.ox.ac.uk/downloads/decision-modelling-for-health-economic-evaluation) (Briggs et al.). This Excel file can be viewed in the freely available software package [Libre Office](https://www.libreoffice.org/).
2. Get started with R using [Swirl](https://swirlstats.com/students.html/) and choose the 'R programming' course when you see it. You can use the [Base R](http://github.com/rstudio/cheatsheets/raw/master/base-r.pdf) cheatsheet as a nice reference.
3. To put what you learned to the test, try to reproduce exercise 2.5 from Briggs et al. in R. You will likely need to learn about logical loops in R which is not covered in the 'R Programming' course in Swirl. A tutorial focussed on this is available on [Datacamp](https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r).
4. Learn intermediate and advanced R topics from the list below (we'll find out which ones will prove to be essential as we go along)
  - [Tidyverse package](https://www.tidyverse.org/) for additional data science tools. Learning either through the book [R for Data Science](https://r4ds.had.co.nz) or the Swirl course [Getting and Cleaning Data](http://swirlstats.com/scn/getclean.html).
  - [Heemod package](https://www.rdocumentation.org/packages/heemod) for tools specific to health economic outcomes research.
  - [Statistical Programming and Applications](https://acaimo.github.io/teaching/MAIN/Stat_Prog_App.html) module from the University of Dublin.
  - [Topics in Applied Statistics](https://acaimo.github.io/teaching/MAIN/Topics_App_Stats.html) module from the University of Dublin.
 5. Learn R shiny to create clean web interface to present your models with. There is an official course over on [Datacamp](https://www.datacamp.com/home/).
 6. Reproduce the Briggs et al. exercise in R Shiny.
 7. Develop your own cool R Shiny HEOR widget.
