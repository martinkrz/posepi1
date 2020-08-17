# Points of Significance: Modeling infectious epidemics

##### Ottar Bjørnstad¹𝄒², Katriona Shea¹, [Martin Krzywinski](mailto:martink@bcgsc.ca)³, Naomi Altman⁴

1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
3. Canada's Michael Smith Genome Sciences Center, Vancouver, British Columbia, Canada.
4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.

Explore [interactive figures](https://shiny.bcgsc.ca/posepi1/) or [download the code](https://martinkrz.github.io/posepi1/) accompanying the column.

![Points of Significance: Modeling infectious epidemics](https://raw.githubusercontent.com/martinkrz/posepi1/master/www/img/screenshot.png)

## Getting Started

### Access remotely

[Online interactive server](https://shiny.bcgsc.ca/posepi1/)

### Downloading code

[GitHub repository](https://martinkrz.github.io/posepi1/)

### Running locally

Download [R Studio](http://rstudio.com) and install packages.

```
install.packages(c("shiny","deSolve","ggplot2","tidyverse","stringr","shinyjs","shinyWidgets"))
```

Run the app by loading `app.R` and cliking `Run App`.

## Shiny app authors

* **Martin Krzywinski** | *Coding*
* **Ottar Bjørnstad** | *Code template and tips*

## Bugs and comments

[Martin Krzywinski](mailto:martink@bcgsc.ca)

## Versions

### v1.0.0

Initial release.

### v1.0.1

Minor text changes and fixed typos.

### v1.0.2

UI tweaks.

### v1.0.3

Added link to SEIRS column.

### v1.0.4

Added link to uncertainty and management column.

## Citation

Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. [Points of Significance: Modelling infectious epidemics](https://www.nature.com/articles/s41592-020-0822-z). (2020) *Nature Methods* **17**:455-456. ([interactive figures](http://shiny.bcgsc.ca/posepi1), [download code](https://martinkrz.github.io/posepi1/))

## Related Columns

Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. [Points of Significance: The SEIRS model for infectious disease dynamics](http://www.nature.com/articles/s41592-020-0856-2). (2020) *Nature Methods* **17**:557-558. ([interactive figures](http://shiny.bcgsc.ca/posepi2), [download code](https://martinkrz.github.io/posepi2/))

Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. Points of Significance: Uncertainty and the management of epidemics. (2020) *Nature Methods* **17** (in press). ([interactive figures](http://shiny.bcgsc.ca/posepi3), [download code](https://martinkrz.github.io/posepi3))

## License

This project is licensed under the [https://creativecommons.org/licenses/by-nc/4.0/](Creative Commons Attribution-NonCommercial 4.0 International Public License) - see the [LICENSE](LICENSE) file for details.
