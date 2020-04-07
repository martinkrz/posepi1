# Points of Significance - Modelling Infectious Epidemics
# Ottar Bjornstad (1,2), Katriona Shea (1), Martin Krzywinski (3*), Naomi Altman (4)
#
# 1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
# 2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
# 3. Canada’s Michael Smith Genome Sciences Centre, Vancouver, British Columbia, Canada.
# 4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.
# * martink@bcgsc.ca
#
# https://martinkrz.shinyapps.io/posepi1/
#
# 31-03-2020 - added log scale to plot 1
#              added ip to figure title
#              figure titles now include "Supplemental"
#              "total epidemic size changed" to "cumulative epidemic size"
#              scale on R0 slider now 1-5 for all panels
#              defaults: R0=3 (panel 1, 2) R0mitigated=2 (panel 2), to match values in columns
# 02-04-2020   added tables
#              in panel 2, intermediate R0 curves are off by default but can be turned on
#              factored out VAR = VALUE strings in text to varfmt()
#              increased time steps to 5000
# 03-04-2020   factored out some functions into {ggplot,format}.helpers.R
#              factored out plot drawing into separate functions that return lists of plots, indexes and titles
#              formatted figure titles
#              added citation masthead
#              added Nat Meth branding masthead
#              added css/style.css as theme for fluidPage()
# 04-04-2020   figure captions
#              more CSS styling

require(shiny)
require(deSolve)
require(ggplot2)
library(grid)
library(stringr)

palette              = c(S="#333333",I="#f15a24",R="#29abe2",G="#8cc63f",C="#000000")
height               = 400 # height for plots
line_plot_width      = 1.5
sir_init_i           = 0.001
sir_system_steps     = 5000
sir_system_time_step = 0.01 # last resort default, shouldn't be used
ip_max               = 28
ip_default           = 14
R0_max               = 5
R0_step              = 0.1

my.plot_legend = list(
  scale_colour_manual("GROUP", 
                      breaks = c("S", "I", "R","C"),
                      labels = c("susceptible","infected","recovered","capacity"),
                      values = palette)
)  

source("R/format.helpers.R",local=TRUE)
source("R/ggplot.helpers.R",local=TRUE)

source("R/ui.R",local=TRUE)
source("R/server.R",local=TRUE)

shinyApp(ui, server)
