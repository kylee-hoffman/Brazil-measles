# Install and load necessary packages
install.packages("remotes")
remotes::install_github("danicat/read.dbc")
remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(dplyr)
states <- "AC"
years <- 2020

# Getting mortality data from SIM (Mortality Information System)
# for each state and year
for (state in states) {
  for (year in years) {
    # Fetch data from DATASUS
    data <- fetch_datasus(year_start = year, year_end = year,
                          uf = states, information_system = "SIM-DO")
    
    # The file is saved locally as "data.csv" on the notebook
    local_file_path <- "data.csv"
    write.csv(data, local_file_path)
  }
}

# Getting mortality data from SIM (Mortality Information System)
# for each state and year
for (state in states) {
  for (year in years) {
    # Fetch data from DATASUS
    data_sinan <- fetch_datasus(year_start = year, year_end = year,
                          uf = states, information_system = "SINAN")
    
    # The file is saved locally as "data.csv" on the notebook
    #local_file_path <- "data.csv"
    #write.csv(data, local_file_path)
  }
}

# trying another source
library(datazoom.amazonia)

# download raw data for the year 2010 in the state of AM.
data_new <- load_datasus(
  dataset = "datasus_sim_do",
  time_period = 2010,
  states = "AM",
  raw_data = TRUE,
  language = "pt"
)

# download treated data with the number of deaths by cause in AM and PA.
data <- load_datasus(
  dataset = "datasus_sim_do",
  time_period = 2010,
  states = c("AM", "PA"),
  raw_data = FALSE
)

# download treated data with the number of deaths by cause in AM and PA
# keeping all individual variables.
data <- load_datasus(
  dataset = "datasus_sim_do",
  time_period = 2010,
  states = c("AM", "PA"),
  raw_data = FALSE,
  keep_all = TRUE
)

data_sim <- load_datasus(
  dataset = "datasus_sim_do",
  time_period = 2010,
  states = c("AM", "PA"),
  raw_data = TRUE,
  keep_all = TRUE
)

## another

install.packages("synapser", repos=c("http://ran.synapse.org", "https://cloud.r-project.org"))
install.packages("synapser")

library(synapser)
synLogin("username", "password")