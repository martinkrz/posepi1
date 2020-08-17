# Points of Significance - Modelling Infectious Epidemics
# Ottar Bjornstad (1,2), Katriona Shea (1), Martin Krzywinski (3*), Naomi Altman (4)
#
# 1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
# 2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
# 3. Canada’s Michael Smith Genome Sciences Centre, Vancouver, British Columbia, Canada.
# 4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.
# * martink@bcgsc.ca
#
# CITATION
# Bjornstad, O., Shea, K., Krzywinski, M. & Altman, N. 
# Points of Significance: Modelling Infectious Epidemics (2020) Nature Methods 17:455-456.
#
# DOWNLOAD
# https://martinkrz.github.io/posepi1
#
# REMOTE ACCESS
#
# http://shiny.bcgsc.ca/posepi1
#
# RUN LOCALLY
#
# 1. Requirements
#
require(shiny)
require(deSolve)
require(ggplot2)
library(stringr)
library(shinyjs)
library(shinyWidgets)
#
# 2. Getting started
#
# Load this file in R Studio and click "Run App" in top right of this pane.
#
# CHANGELOG
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
# 06-04-2020   factored out ui.R, server.R
#              created GitHub repo https://github.com/martinkrz/posepi1
# 07-04-2020   Renamed some variables.
#              Streamlined and updated CSS.
#              Removed fixed plot height and aspect ratio
# 09-04-2020   added debug timings
#              reduced time steps to 1000 to decrease load on server
# 06-05-2020   minor text changes and fixed typos
# 13-05-2020   UI tweaks
# 18-06-2020   added link to SEIRS column

# CUSTOM SETTINGS
# The colors of the suscetible (S), infected  (I) and recovered (R) trajectories. 
palette              = c(S="#333333",I="#f15a24",R="#29abe2",G="#8cc63f",C="#333333")
# Plot height and line width for trajectories
plot_line_width      = 1.5
plot_text_size       = 12
# Initial value for I(0). S(0) = 1 - sir_init_i - vaccination_fraction and R(0) = vaccination_fraction
sir_init_i           = 0.001
sir_system_steps     = 1000
sir_system_time_step = 0.01 # last resort default, shouldn't be used
# Infectious period max and default. Slider step for ip is 1.
ip_max               = 28
ip_default           = 14
# R0 max and slider step
R0_max               = 5
R0_step              = 0.1
# do timings
do_timing            = FALSE

interpretive_default = FALSE
captions_default     = FALSE

my.plot_legend = list(
  scale_colour_manual("GROUP", 
                      breaks = c("S", "I", "R","C"),
                      labels = c("susceptible","infected","recovered","capacity"),
                      values = palette)
)  

source("R/format.helpers.R",local=T)
source("R/ggplot.helpers.R",local=T)
source("R/ui.R",local=T)
source("R/server.R",local=T)

shinyApp(ui,server)
