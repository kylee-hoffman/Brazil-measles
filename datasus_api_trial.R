# Install and load necessary packages
install.packages("remotes")
remotes::install_github("danicat/read.dbc")
remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)

states <- "AC"
years <- 2020

# Getting mortality data from SIM (Mortality Information System)
# for each state and year
for (state in states) {
  for (year in years) {
    # Fetch data from DATASUS
    data <- fetch_datasus(year_start = year, year_end = year,
                          uf = states, information_system = "SINAN-DENGUE")
    
    # The file is saved locally as "data.csv" on the notebook
    local_file_path <- "data.csv"
    write.csv(data, local_file_path)
  }
}
