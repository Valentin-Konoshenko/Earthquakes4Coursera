##### checks ####
if (!require(readr)) {
  install.packages("readr");
  if(!require(readr, character.only = TRUE)) stop("Package readr not found");
}
if (!require(dplyr)) {
  install.packages("dplyr");
  if(!require(dplyr, character.only = TRUE)) stop("Package dplyr not found");
}
if (!require(ggplot2)) {
  install.packages("ggplot2");
  if(!require(ggplot2, character.only = TRUE)) stop("Package ggplot2 not found");
}
if (!require(lubridate)) {
  install.packages("lubridate");
  if(!require(lubridate, character.only = TRUE)) stop("Package lubridate not found");
}
if (!require(grid)) {
  install.packages("grid");
  if(!require(grid, character.only = TRUE)) stop("Package grid not found");
}
if (!require(leaflet)) {
  install.packages("leaflet");
  if(!require(leaflet, character.only = TRUE)) stop("Package leaflet not found");
}
