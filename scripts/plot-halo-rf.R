# Plot the halocarbon radiative forcings.

# 0. Set Up --------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)
library(rgcam)
library(hector)
library(tidyr)

# Data vis settings
theme_set(theme_bw())
num <- 1
show_col(hue_pal()(num))
selected_colors <- hue_pal()(num)

COLORS <- selected_colors
names(COLORS) <- c("new")
COLORS <- c(COLORS, "old" = "black")

# Size of the plots to save
W <- 6 ; H <- 4


# Directories
FIGS_DIR <- "figs"
GCAM_CORE <- "~/Documents/GCAM-WD/gcam-core"



# Helper function that quick generates comparison plots
# Args
#   project.data: object created by rgcam from a GCAM xml output db
#   VAR: string variable
# Retunrs: ggplot comparing the results
my_quick_plot <- function(project.data, VAR){

  getQuery(project.data, VAR) ->
    to_plot

  stopifnot(nrow(to_plot) > 5)

  unit <- unique(to_plot$Units)


  to_plot %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    summarise(MAE = mean(abs(new - old))) ->
    MAE


  to_plot %>%
    filter(year > 1975) %>%
    ggplot(aes(year, value, color = scenario, linetype = scenario)) +
    geom_line() +
    scale_color_manual(values = COLORS) +
    labs(title = VAR, x = NULL, y = unit,
         subtitle = paste0("MAE: ", MAE[[1]])) +
    theme(legend.title = element_blank()) ->
    out

  return(out)

}



# 1. Extra Data --------------------------------------------------------------------

# Extract the results from the gcam data base
conn  <- localDBConn(file.path(GCAM_CORE, "output"), 'database_basexdb')
query_file <- "climate-queries.xml"
project.data <- addScenario(conn, 'project.dat', scenario = "new", queryFile = query_file)
listQueries(project.data)

# Plot results
my_quick_plot(project.data, "RF_tot") %>%
  ggsave(filename = file.path(FIGS_DIR, "RF_tot.png"), width = W, height = H)

my_quick_plot(project.data, "gmst") %>%
  ggsave(filename = file.path(FIGS_DIR, "gmst.png"), width = W, height = H)

my_quick_plot(project.data, "RF_CF4") %>%
  ggsave(filename = file.path(FIGS_DIR, "RF_CF4.png"), width = W, height = H)

my_quick_plot(project.data, "Montreal RF") %>%
  ggsave(filename = file.path(FIGS_DIR, "Montreal_RF.png"), width = W, height = H)

my_quick_plot(project.data, "PFCs RF")  %>%
  ggsave(filename = file.path(FIGS_DIR, "PFC_RF.png"), width = W, height = H)













