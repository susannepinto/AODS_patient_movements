P75 with patient movements
================
Susanne Pinto
2025-05

``` r
rm( list = ls() ) # remove working environment
```

Below you can choose your pathogen and location, for the pathogen choose
from: “Acinetobacter spp.”, “Acinetobacter Baumannii (Meropenem
resistant)”, “Acinetobacter Baumannii (Quinolone and Aminoglycoside
resistant)”, “Enterobacteriaceae (Meropenem resistant)”,
“Enterobacteriaceae (Quinolone and Aminoglycoside resistant, excluding
Escherichia coli)”, “Enterobacter cloacae (ESBL)”, “Enterobacter cloacae
complex (ESBL)”, “Escherichia coli (Ciprofloxacin resistant)”,
“Escherichia coli (ESBL)”, “Citrobacter freundii (Ciprofloxacin
resistant)”, “Klebsiella pneumoniae (ESBL)j”, “Klebsiella oxytoca
(ESBL)”, “Serratia marcescens”, “Pseudomonas aeruginosa”, “MDR
Pseudomonas aeruginosa”, “Stenotrophomonas maltophila (Cotrimoxazole
resistant)”, “Stenotrophomonas maltophila”, “Group A streptococcal
infection (GAS)”, “Colistin resistance in bacteria”,
“Penicillin-Resistant Streptococcus pneumoniae (PRSP)”,
“Meticillin-Resistant Staphylococcus aureus (MRSA)”,
“Meticillin-Susceptible Staphylococcus aureus (MSSA)”,
“Vancomycin-resistant Enterococci (VRE)”, “Bacillus spp.”, “Candida spp.
(fluconazole resistant in blood cultures)”, “Candida norvegensis”,
“Aspergillus fumigatus”

and for the location choose from: “U440” = “ICU cluster”, “A300” =
“Internal medicine cluster”, “D360” = “Neurology cluster”, “C302” =
“Surgery cluster”, “B410” = “Cardiology cluster”, “F311” = “Neonates and
NICU cluster”, “F710” = “Mothers cluster”, “I330” = “Children cluster”)

``` r
# Selected species (see below for the possibilities)
selected_species <- "MDR Pseudomonas aeruginosa"  

# Define the specific location
# Used for base case and individual patient movements
specific_location <- "U440"
```

# General introduction

In this document we step-by-step introduce the different possibilities
of incorporating patient movements (on group and individual level) to
the P75 system:

- Base case situation (without patient movements). Results are given for
  the currently investigated ward and all wards

- P75 based on Louvain clusters. Results are given for the different
  clusters and for all wards

- P75 extended with individual patients movements in the 14 days prior
  to the positive test. Every prior location is awarded a point

- P75 extended with individual patients movements in the 14 days prior
  to the positive test. Every prior location is awarded a point per day

We simulate that the systems were used every week (on Tuesday), looking
back and using the counts of the previous 30 days. The threshold is
calculated based on those weekly counts. Note that this is different
from the previous deliverable where monthly counts were used to
calculate the threshold.

A cluster is determined afterwards and is different from the counts that
causes an alarm. If an alarm is returned, the first patient involved in
that alarm is the first patient of a cluster. Every next patient within
60 days, who had stayed at the same location (or within a location
belonging to the same Louvain cluster) is part of that cluster. Note
that, if there was another patient with a positive test that caused an
alarm within the next 30 days again, this alert was also assigned to
that same cluster including all patients that were within 60 days (30
prior and 30 past) of this new alert.

We added two days to the collection date of the test, because test
results come back later.

  

``` r
library( here )
library( tidyverse )
library( tidyr )
library( haven ) # to import SAS en spss files
library( reshape2 ) # data from long to wide format
library( dplyr ) # remove duplicated rows
library( readxl ) # To read in excel data
library( lubridate ) # To deal with dates
library( stringr ) # Adjust strings in datasets
library( ggplot2 ) # Make the alert graphs
library( knitr ) # Show tables nicely
library( kableExtra ) # Show tables nicely
library( conflicted ) # Functions with the same name in the CLAR package
library( ggpubr ) # To show multiple CLAR plots as 1 plot
library( VennDiagram ) # Venn plot
library( cowplot ) # aranging plots
library( anytime ) # work with time in rows
library( data.table) # allows to generate "run-length" groups directly
library( patchwork ) # to combine plots
library( writexl ) # save tables
library( rlang )
library( knitr )
library( zoo )

# some packages have the same function, specify which package to use
conflicts_prefer( dplyr::first )
conflicts_prefer( dplyr::filter )
conflicts_prefer( dplyr::lag )
conflicts_prefer( lubridate::year )
conflicts_prefer( lubridate::month )
conflicts_prefer( lubridate::week )
conflicts_prefer( lubridate::days )
conflicts_prefer( base::union )
conflicts_prefer( cowplot::align_plots )
```

``` r
# Below the laboratory data, excluding patients who objected to the use of their data, is loaded
# Also to every AfnameDatum two extra dates are added, because test results will come back after some time, and not on the same day

# load the joined data
#load(here("Patient_movements", "Data", "glimsbezwaar.RData"))

# Sort the data
glimsbezwaar <- glimsbezwaar %>% 
  arrange( AfnameDatum_LAB, studyId_outbreakdetec_Patient, studyId_outbreakdetec_LabID, IsolaatNaam_LAB, Startdatum_OPNMUT, Einddatum_OPNMUT )

# Remove duplicates and keep the upper one
# This function does not do anything, because the data already does not contain any duplicates any more
gold <- glimsbezwaar %>% 
  distinct( AfnameDatum_LAB, studyId_outbreakdetec_Patient, studyId_outbreakdetec_LabID, IsolaatNaam_LAB, Startdatum_OPNMUT, .keep_all = TRUE )

# Add 2 days to AfnameDatum, because test results come back later
gold$AfnameDatum_LAB_original <- gold$AfnameDatum_LAB
gold$AfnameDatum_LAB <- gold$AfnameDatum_LAB + days( 2 )

# Sort the data
gold <- gold %>% arrange( IsolaatNaam_LAB )

# clean environment
rm( glimsbezwaar )
```

``` r
# This script loads the excelsheet that indicates whether a pathogen is an enterobacter. 
# Not always needed, but for some species it is needed

# Read in the data
#entbact <- read_excel(here("Patient_movements", "Data", "MicroOrganism.xlsx"))

# Make another dataset and filter the entbact's
entbact1 <- subset( entbact, entbact == "Y" )
# Rename a column, and give it the same name as in the Gold dataset
entbact2 <- entbact1
colnames( entbact2 )[1] <- "IsolaatNaam_LAB" # change Mnemonic
# Sort the data
entbact2 <- entbact2 %>% arrange( IsolaatNaam_LAB )
# Merge the data
goldentbact <- merge( gold, entbact2, by = c( "IsolaatNaam_LAB" ), all = TRUE )

# clean environment
rm( entbact, entbact1 )
```

  

# Include patient movements

  

Some patient movements in the dataset were not physical movements, but
represented for example administrative events, e.g. when a patient
received treatment from another department (e.g. dialysis), while
staying in the same place or when a movement attempt failed, but the bed
was already reserved. These transfers can be recognized by the short
transfer time. Therefore, we removed admissions to wards with a length
less than four hours. Moreover, when a patient was transferred from one
department to another, we counted this transfer as a single movement. If
a patient returned to a previously visited ward, we counted this
transfer as a new movement. However, consecutive patient movements
within a single department were not considered in this study.

``` r
# The script below reads in the movement data, and remove the patients that were not in the Laboratory data

# Read in the opname data
#Patient_movements <- read_sas(here("Patient_movements", "Data", "opnames_pseudo20230725.sas7bdat"))

# Rearrange the columns so pseudo-IDs are the first colums
Patient_movements <- Patient_movements[, c( 1, 14, 13, 2:12 )]
#colnames( Patient_movements )

# Sort the data (rows)
Patient_movements1 <- Patient_movements %>% 
   arrange( studyId_outbreakdetec_Patient, studyId_outbreakdetec_Hos_ID, hos_start_dt, hos_stop_dt, hos_mut_start_dt, hos_mut_stop_dt )

# Read in the voorkeurs data
#preferredID <- read_sas(here("Patient_movements", "Data", "voorkeurs_id.sas7bdat"))

# Sort the data (rows)
preferredID <- preferredID %>% 
   arrange( studyId_outbreakdetec_Pat_voork, studyId_outbreakdetec_Patient )

# Filtering rows in Patient_movements1 based on preferredID
# Patient_movements1.filtered will now contain rows from Patient_movements1 where studyId_outbreakdetec_Patient is not in preferredID
Patient_movements1.filtered <- Patient_movements1 %>%
  filter( !studyId_outbreakdetec_Patient %in% preferredID$studyId_outbreakdetec_Patient ) # removed 74666 rows

# clean environment
rm( Patient_movements, preferredID )
```

``` r
# The script below removes the pseudo or failed attempt movements, i.e. short movements >= 4 hour.
# Also sequential movements within a ward are merged and not be seen as unique movements afterwards.
# Note that this data is not used in the base case, or for the Louvain clustering analysis

# The getTZ function returns the timezone values stored in local package environment, and set at package load time. Also note that this argument applies to the output: the returned object will have this timezone set. The timezone is not used for the parsing which will always be to localtime, or to UTC is the asUTC variable is set
Patient_movements2 <- Patient_movements1.filtered
Patient_movements2$hos_mut_start_dt <- anytime( Patient_movements2$hos_mut_start_dt, asUTC = TRUE )
Patient_movements2$hos_mut_stop_dt <- anytime( Patient_movements2$hos_mut_stop_dt, asUTC = TRUE )

# Calculate the time difference in hours
time_difference <- as.numeric( difftime(Patient_movements2$hos_mut_stop_dt, Patient_movements2$hos_mut_start_dt, units = "hours" ))
# Keep only rows where the time difference is greater than or equal to 4 hours
Patient_movements3 <- Patient_movements2[ time_difference >= 4, ]

# Remove rows where hos_mut_Afd_code is NA
Patient_movements3 <- Patient_movements3[ complete.cases( Patient_movements3$hos_mut_Afd_code ), ] 

# Convert date columns to Date objects
Patient_movements4 <- Patient_movements3 %>%
  mutate(
    hos_start_dt = as.POSIXct( hos_start_dt ),
    hos_stop_dt = as.POSIXct( hos_stop_dt ),
    hos_mut_start_dt = as.POSIXct( hos_mut_start_dt ),
    hos_mut_stop_dt = as.POSIXct( hos_mut_stop_dt )
  )

## Merge movements that indicate transfers within wards
# Note that the script below only merges consecutive movements within wards, if a patient moves back to a previous ward after being at another ward, this information is kept. 

# Create a grouping variable based on changes in specified columns
Patient_movements4 <- Patient_movements4 %>%
  mutate(group_id = rleid(
    studyId_outbreakdetec_Patient,
    studyId_outbreakdetec_Hos_ID,
    hos_start_dt,
    hos_stop_dt,
    hos_mut_specialism,
    hos_mut_Afd_code,
    hos_mut_afd_Omschrijving
  ))

# Merge consecutive rows within the same ward
# Keep the earliest date of "hos_mut_start_dt" and the eldest date of "hos_mut_stop_dt"
Patient_movements4_merged <- Patient_movements4 %>%
  group_by( group_id ) %>%
  summarise(
    studyId_outbreakdetec_Patient = first( studyId_outbreakdetec_Patient ),
    studyId_outbreakdetec_Hos_ID = first( studyId_outbreakdetec_Hos_ID ),
    hos_start_dt = first( hos_start_dt ),
    hos_stop_dt = first( hos_stop_dt ),
    hos_mut_specialism = first( hos_mut_specialism ),
    hos_mut_Afd_code = first( hos_mut_Afd_code ),
    hos_mut_afd_Omschrijving = first( hos_mut_afd_Omschrijving ),
    hos_mut_start_dt = min( hos_mut_start_dt ),
    hos_mut_stop_dt = max( hos_mut_stop_dt )
  ) %>%
  ungroup()

# Remove the group_id column
Patient_movements4_merged <- select( Patient_movements4_merged, -group_id ) 

# clean environment
rm( Patient_movements1, Patient_movements1.filtered, Patient_movements2, Patient_movements3, Patient_movements4, time_difference )
```

  

# Define Louvain clusters

  

Note that wards are opened and closed over time and that patient
movements can also differ over time. Therefore, the clusters are also
different over the years.

``` r
# Set Louvain clusters, these clusters were defined with an earlier network study

# load the joined data
# Louvain.cluster.results <- read.csv(
#   here("Patient_movements", "Data", "Louvain.cluster.results.csv"),
#   header = TRUE,
#   sep = ";",
#   stringsAsFactors = FALSE
# )

# Create the new column Louvain.clusters
Louvain.cluster.results <- Louvain.cluster.results %>%
  mutate(Louvain_cluster = paste("Cluster", Community, Hospital, Year))

# Filter out rows where Year is "AllYears"
Louvain.cluster.results <- Louvain.cluster.results %>%
  filter(Year != "AllYears")

# Create a new column for the previous year's Louvain cluster value
Louvain.cluster.results <- Louvain.cluster.results %>%
  arrange(Ward, Year) %>%
  group_by(Ward) %>%
  mutate(PrevYear_Louvain_cluster = dplyr::lag(Louvain_cluster))

# Fill missing values in PrevYear_Louvain_cluster in 2014 with the last known value
Louvain.cluster.results <- Louvain.cluster.results %>%
  mutate(PrevYear_Louvain_cluster = ifelse(Year == 2014 & is.na(PrevYear_Louvain_cluster),
                                          Louvain_cluster,
                                          PrevYear_Louvain_cluster)) %>%
  ungroup()

# Create lookup table (unique Ward name and corresponding code)
lookup_table <- Patient_movements4_merged %>%
  distinct(hos_mut_afd_Omschrijving, hos_mut_Afd_code)

# Join with Louvain.cluster.results to replace Ward with corresponding code
Louvain.cluster.results <- Louvain.cluster.results %>%
  left_join(lookup_table, by = c("Ward" = "hos_mut_afd_Omschrijving")) %>%
  mutate(Ward = hos_mut_Afd_code) %>%
  select(-hos_mut_Afd_code)  # remove the extra column

# Sort the data (rows)
Louvain.cluster.results <- Louvain.cluster.results %>% 
   arrange( Year, Ward )

# Select only the necessary columns from Louvain.cluster.results for merging
Louvain.cluster.results <- Louvain.cluster.results %>%
  select(Ward, Year, PrevYear_Louvain_cluster)

names(Louvain.cluster.results)[names(Louvain.cluster.results) == "Ward"] <- "location"
names(Louvain.cluster.results)[names(Louvain.cluster.results) == "PrevYear_Louvain_cluster"] <- "Louvain_cluster"

# Define mapping of locations to new cluster names
location_mapping <- c("U440" = "ICU cluster",
                      "A300" = "Internal medicine cluster",
                      "D360" = "Neurology cluster",
                      "C302" = "Surgery cluster",
                      "B410" = "Cardiology cluster",
                      "F311" = "Neonates and NICU cluster",
                      "F710" = "Mothers cluster",
                      "I330" = "Children cluster")

# Define the locations you want to use as references
reference_locations <- c("U440", "A300", "D360", "C302", "B410", "F311", "F710", "I330")

# Create a lookup table from df based on reference_locations
lookup_table <- Louvain.cluster.results %>%
  filter(location %in% reference_locations) %>%
  distinct(location, Louvain_cluster) %>%
  mutate(Louvain_cluster2 = location_mapping[location]) %>%
  select( location, Louvain_cluster, Louvain_cluster2)

Louvain.cluster.results <- Louvain.cluster.results %>%
  left_join(lookup_table, by = c("Louvain_cluster"))

# Update Louvain_cluster2 based on location and Louvain_cluster
Louvain.cluster.results <- Louvain.cluster.results %>%
  select(-c(location.y)) %>%
  distinct()  # Remove duplicated rows

Louvain.cluster.results <- Louvain.cluster.results %>% 
  mutate(Louvain_cluster2 = ifelse(is.na(Louvain_cluster2), "closed", Louvain_cluster2))

Louvain.cluster.results <- Louvain.cluster.results %>%
  rename(location = location.x)

Louvain.cluster.results <- Louvain.cluster.results %>%
  select(-c(Louvain_cluster))
Louvain.cluster.results <- Louvain.cluster.results %>%
  rename(Louvain_cluster = Louvain_cluster2)
```

``` r
#selected.locations <- c("B531", "B430", "B330", "B340", "U440", "F311", "F341", "F342", "F343", "F344", "F310", "F312", "F710")

# Extract the cluster of the specific location
specific_cluster <- Louvain.cluster.results %>%
  filter(location == specific_location) %>%
  select(Louvain_cluster) %>%
  pull() %>% 
  unique()

specific_cluster <- specific_cluster[1]

# Select all rows belonging to the same cluster as the specific location
same_cluster_locations <- Louvain.cluster.results %>%
  filter(Louvain_cluster == specific_cluster)

# Used for louvain clustering
selected.locations <- unique(same_cluster_locations$location)

rm( same_cluster_locations)
```

  
  

## Selection of pathogen

  

``` r
# The script below filters the complete dataset (with all pathogens/locations) 
# Create two new columns (month and year) based on AfnameDatum_LAB
# Than filter the data based on the conditions given

# Define the full path to the selection scripts
#script_path <- here("Patient_movements", "scripts", "selection_scripts.R")
# Source the selection scripts using the full path
source(script_path)

# Selection based on the species
if (selected_species == "Acinetobacter spp.") {
  gold_select <- select_Acinetobacter_spp(gold)
} else if (selected_species == "Acinetobacter Baumannii (Meropenem resistant)") {
  gold_select <- select_Acinetobacter_meroR(gold)
} else if (selected_species == "Acinetobacter Baumannii (Quinolone and Aminoglycoside resistant)") {
  gold_select <- select_Acinetobacter_ciproR(gold)
} else if (selected_species == "Enterobacteriaceae (Meropenem resistant)") {
  gold_select <- select_Enterobacteriaceae_meroR(goldentbact)
} else if (selected_species == "Enterobacteriaceae (Quinolone and Aminoglycoside resistant, excluding Escherichia coli)") {
  gold_select <- select_Enterobacteriaceae_quinaminR(goldentbact)
} else if (selected_species == "Enterobacter cloacae (ESBL)") {
  gold_select <- select_Enterobacter_ESBL(gold)
} else if (selected_species == "Enterobacter cloacae complex (ESBL)") {
  gold_select <- select_Enterobacter_ESBLcomplex(gold)
} else if (selected_species == "Escherichia coli (Ciprofloxacin resistant)") {
  gold_select <- select_Ecoli_ciproR(gold)
} else if (selected_species == "Escherichia coli (ESBL)") {
  gold_select <- select_Ecoli_ESBL(gold)
} else if (selected_species == "Citrobacter freundii (Ciprofloxacin resistant)") {
  gold_select <- select_Citrobacter_ciproR(gold)
} else if (selected_species == "Klebsiella pneumoniae (ESBL)") {
  gold_select <- select_Klebsiella_pneumoniae_ESBL(gold)
} else if (selected_species == "Klebsiella oxytoca (ESBL)") {
  gold_select <- select_Klebsiella_oxytoca_ESBL(gold)
} else if (selected_species == "Serratia marcescens") {
  gold_select <- select_Serratia_marcescens(gold)
} else if (selected_species == "Pseudomonas aeruginosa") {
  gold_select <- select_Pseudomonas_aeruginosa(gold)
} else if (selected_species == "MDR Pseudomonas aeruginosa") {
  gold_select <- select_mdr_Pseudomonas_aeruginosa(gold)
} else if (selected_species == "Stenotrophomonas maltophila (Cotrimoxazole resistant)") {
  gold_select <- select_Stenotrophomonas_maltophila_cotR(gold)
} else if (selected_species == "Stenotrophomonas maltophila") {
  gold_select <- select_Stenotrophomonas_maltophila(gold)
} else if (selected_species == "Group A streptococcal infection (GAS)") {
  gold_select <- select_GAS(gold)
} else if (selected_species == "Colistin resistance in bacteria") {
  gold_select <- select_colistinR(gold)
} else if (selected_species == "Penicillin-Resistant Streptococcus pneumoniae (PRSP)") {
  gold_select <- select_PRSP(gold)
} else if (selected_species == "Meticillin-Resistant Staphylococcus aureus (MRSA)") {
  gold_select <- select_MRSA(gold)
} else if (selected_species == "Meticillin-Susceptible Staphylococcus aureus (MSSA)") {
  gold_select <- select_MSSA(gold)
} else if (selected_species == "Vancomycin-resistant Enterococci (VRE)") {
  gold_select <- select_VRE(gold)
} else if (selected_species == "Bacillus spp.") {
  gold_select <- select_Bacillus(gold)
} else if (selected_species == "Candida spp. (fluconazole resistant in blood cultures)") {
  gold_select <- select_Candida_bloodcultures(gold)
} else if (selected_species == "Candida norvegensis") {
  gold_select <- select_Candida_norvegensis(gold)
} else if (selected_species == "Aspergillus fumigatus") {
  gold_select <- select_Aspergillus_fumigatus(gold)
}

# clean environment
rm( script_path )
rm(list = ls(pattern = "^select_"))
```

``` r
# Create a combined date column
plot_dataset <- gold_select %>%
  mutate(month = sprintf("%02d", as.numeric(month)),
         Date = as.Date(paste(year, month, "01", sep = "-")))

# Count occurrences per month/year for the first plot
plot_dataset_count <- plot_dataset %>% 
  count(Date)

filtered_plot_dataset <- plot_dataset %>%
  filter(location %in% selected.locations)

# Count occurrences per month/year for the second plot
filtered_plot_dataset_count <- filtered_plot_dataset %>%
  count(Date)

# Determine the y-axis limits
y_max <- max(plot_dataset_count$n, filtered_plot_dataset_count$n)

# Generate the first plot
plot1 <- ggplot(data = plot_dataset_count, aes(x = Date, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Date", y = "Count") +
  geom_text(aes(label = n), vjust = -0.5, color = "red", size = 4) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(limits = c(0, y_max)) +
  ggtitle(selected_species) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Generate the second plot
plot2 <- ggplot(data = filtered_plot_dataset_count, aes(x = Date, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Date", y = "Count") +
  geom_text(aes(label = n), vjust = -0.5, color = "red", size = 4) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(limits = c(0, y_max)) +
  ggtitle(paste(selected_species, "in", specific_cluster)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots
combined_plot <- plot1 + plot2

# Display the combined plot
print(combined_plot)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/plot%20incidences-1.png)<!-- -->

``` r
# Define the file path to save the plot
file_path <- file.path("~/Documents/AODS_Susanne/Patient_movements/Results/Results patient movements", selected_species, specific_cluster, "P75/Descriptive results/combined_plot.png")

# Save the combined plot
#ggsave(file_path, combined_plot, width = 14, height = 7)

# clean environment
rm( plot_dataset )
```

**Fig. 1** MDR Pseudomonas aeruginosa species abundances in the UMCU and
WKZ per month and year. And for the ICU cluster.

  

# P75 system

 

The current system being utilized in the UMC Utrecht summarizes the
monthly count of a specific microorganism, bug-drug combination and/or
units under surveillance. The threshold for an alert is based on the
75<sup>th</sup> percentile (75% of the observations are below that
value) of monthly counts for the preceding year and is redetermined
every year.

  

#### P75 system - base case thresholds (per department)

  

In the current surveillance, only the adult ICU is monitored, however,
with a new system it might be possible to monitor all wards. Moreover,
it makes comparison to the other scenarios that make use of patients’
movements.

  

``` r
# Below we add the count per month, week and 30 days to the dataset 
# Also it removes repeated samples (within a year) per patient.
# Also rows of multiple patients (in the same location) that has the same AfnameDatum are merged together

# Remove duplicated rows
# Keep samples that are more than 1 year apart
goldPA <- gold_select %>% 
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 |
    as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365
  )

# Convert AfnameDatum_LAB to a proper date format
goldPA$AfnameDatum_LAB <- as.Date(goldPA$AfnameDatum_LAB)
goldPA <- goldPA[order(goldPA$AfnameDatum_LAB),]

# Group by location and date, then calculate the cumulative count
goldPAR <- goldPA %>%
  arrange(AfnameDatum_LAB) %>%
  group_by(location, month = format(AfnameDatum_LAB, "%Y-%m")) %>%
  mutate(count_per_month = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB, location, count_per_month)

goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB) %>%
  mutate(week = format(AfnameDatum_LAB - lubridate::wday(AfnameDatum_LAB, week_start = 3) + 1, "%Y-%U")) %>% # per month: month = format(AfnameDatum_LAB, "%Y-%m")
  group_by(location, week) %>% 
  mutate(count_per_week = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB, location, count_per_month, count_per_week)

# First calculate the dates from the first next Tuesday and the previous 30 days
goldPAR <- goldPAR %>%
  mutate(next_tuesday = AfnameDatum_LAB + lubridate::days((9 - (lubridate::wday(AfnameDatum_LAB) - 1) %% 7) %% 7),
         thirty_days_ago = next_tuesday - lubridate::days(30))

# Calculate count_per_30days
goldPAR2 <- goldPAR %>%
  left_join(goldPAR, by = "location") %>%
  group_by(location, studyId_outbreakdetec_Patient.x, AfnameDatum_LAB.x) %>%
  summarize(count_per_30days = sum(AfnameDatum_LAB.y >= thirty_days_ago.x & AfnameDatum_LAB.y <= next_tuesday.x),
            .groups = "drop")

# Change colnames
colnames(goldPAR2) <- c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB", "count_per_30days")

# Add the counts from the 30 previous days
goldPARR <- merge(goldPAR, goldPAR2[, c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB", "count_per_30days")],
                   by = c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB"),
                   all.x = TRUE) %>% 
  arrange(AfnameDatum_LAB, studyId_outbreakdetec_Patient)
 
# Merge rows with the same AfnameDatum_LAB
# Patients IDs are combined, separated by a _, the max count is used for that row
goldPARR <- goldPARR %>%
  group_by(AfnameDatum_LAB, location) %>%
  summarize( studyId_outbreakdetec_Patient = paste( unique( studyId_outbreakdetec_Patient ), collapse = '_' ),         
    count_per_month = max( count_per_month ),
             count_per_week = max( count_per_week ),
             count_per_30days = max( count_per_30days ))

# Add columns for month and year
goldPARR$month <- format(goldPARR$AfnameDatum_LAB, "%m")
goldPARR$year <- format(goldPARR$AfnameDatum_LAB, "%Y")

# clean environment
rm( goldPAR )
```

The P75 threshold is calculated based on the counts (calculated per 30
days) per location of the previous year.

``` r
# This script calculates the thresholds per year per location

# Add the weeknumber to the dataset
goldPARR <- goldPARR %>%
  mutate( week_number = week( AfnameDatum_LAB ))

# Calculate the max. count per week/year/location
goldPARR2 <- goldPARR %>%
  group_by( week_number, year, month, location ) %>%
  summarize( count_per_30days = max( count_per_30days ))

# Calculate the threshold per year and location
goldPA.P75 <- goldPARR2 %>%
  group_by( year, location ) %>%
  summarize( Quantile_75 = quantile( count_per_30days, probs = 0.75 )) 

# Create a new column for the previous year's p75 value
goldPA.P75 <- goldPA.P75 %>%
  arrange(location, year) %>%
  group_by(location) %>%
  mutate(PrevYear_P75 = dplyr::lag(Quantile_75))

# Sort the data
goldPA.P75 <- goldPA.P75 %>% arrange( year, location )

# clean environment
rm( goldPARR2 )
```

``` r
# The script below adds the alarms to the data
# Because we simulate that the system is used every Tuesday all information is moved to that Tuesday.

# Generate a sequence of dates from January 2014 to December 2021
date_sequence <- seq( as.Date( "2014-01-01" ), as.Date( "2021-12-31" ), by = "day" )
# Create a dataframe with all possible dates
all_dates <- data.frame( AfnameDatum_LAB = date_sequence )
# Merge the filtered dataframe with all_dates, filling missing values with 0
goldPARR_merged <- merge( all_dates, goldPARR, by = "AfnameDatum_LAB", all.x = TRUE )
goldPARR_merged$count_per_30days[ is.na( goldPARR_merged$count_per_30days )] <- 0
goldPARR_merged$count_per_week[ is.na( goldPARR_merged$count_per_week )] <- 0
goldPARR_merged$count_per_month[ is.na( goldPARR_merged$count_per_month )] <- 0

# Fill in the new rows for month and year
goldPARR_merged$month <- format(goldPARR_merged$AfnameDatum_LAB, "%m")
goldPARR_merged$year <- format(goldPARR_merged$AfnameDatum_LAB, "%Y")

# Merge with goldPA.P75 to add PrevYear_P75 for each year
goldPARR_merged <- goldPARR_merged %>% left_join(goldPA.P75, by = c("year", "location"))
# Create a new column 'Above_Quantile' to represent whether count is above PrevYear_P75
goldPARR_merged$Above_Quantile <- goldPARR_merged$count_per_30days > goldPARR_merged$PrevYear_P75
# Remove the column with the threshold of that year (we use the one of the previous year)
goldPARR_merged <- select(goldPARR_merged, -c("Quantile_75" ))

# Create a new dataframe and add weekdays
goldPARR_alarms <- goldPARR_merged
goldPARR_alarms$day_of_week <- weekdays(goldPARR_alarms$AfnameDatum_LAB)
goldPARR_alarms$day_of_week <- factor(goldPARR_alarms$day_of_week, levels = c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday"))

# Group by week and check if there was any Above_Quantile == TRUE in the previous week (including that day)
# Add all the variables from a certain week on the Tuesday
goldPARR_alarms <- goldPARR_alarms %>%
  arrange(AfnameDatum_LAB) %>%
  mutate(week_end = as.Date(AfnameDatum_LAB) + 6 - (as.numeric(format(AfnameDatum_LAB, "%u")) - 3) %% 7) %>%
  group_by(week_end, location) %>%
  mutate(
    Alarm_per_week = ifelse(any(Above_Quantile == TRUE) & !all(Above_Quantile == FALSE | is.na(Above_Quantile)), TRUE, 
                            ifelse(all(is.na(Above_Quantile)), NA, FALSE)),
    count_per_week = max(count_per_week),
    count_per_month = max(count_per_month),
    count_per_30days = max(count_per_30days)
  ) %>%
    ungroup() %>%
  group_by(week_end) %>%
  mutate(
    location_per_week = toString(na.omit(location)),
    studyId_outbreakdetec_Patient_per_week = toString(na.omit(studyId_outbreakdetec_Patient)),
    AfnameDatum_LAB_per_week = toString(ifelse(!is.na(location), as.character(AfnameDatum_LAB), "ABSENT")),
    Alarm_per_week2 = toString(ifelse(!is.na(location), Alarm_per_week, "ABSENT")),
    count_per_week2 = toString(ifelse(count_per_week != 0, count_per_week, NA)),
    count_per_month2 = toString(ifelse(count_per_month != 0, count_per_month, NA)),
    count_per_30days2 = toString(ifelse(count_per_30days != 0, count_per_30days, NA))
  ) %>%
  mutate(
    count_per_week2 = gsub(", NA", "", count_per_week2),
    count_per_month2 = gsub(", NA", "", count_per_month2),
    count_per_30days2 = gsub(", NA", "", count_per_30days2),
    count_per_week2 = gsub("NA, ", "", count_per_week2),
    count_per_month2 = gsub("NA, ", "", count_per_month2),
    count_per_30days2 = gsub("NA, ", "", count_per_30days2),
    count_per_week2 = gsub("NA", "", count_per_week2),
    count_per_month2 = gsub("NA", "", count_per_month2),
    count_per_30days2 = gsub("NA", "", count_per_30days2)
  )

# clean environment
rm( date_sequence, all_dates, goldPARR, goldPARR_merged )
```

``` r
# Below the cluster number and cluster ranges are added. This is done per location
# Note that the count that caused the alarm is different from the number of patients belonging to a cluster.
# A cluster is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store cluster numbers
cluster_numbers <- rep(NA, nrow(goldPARR_alarms))
cluster_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB[i]
  current_location <- goldPARR_alarms$location[i]
  
  # Check if cluster number is already assigned within 30 days of the current row's date and same location
  if (is.na(cluster_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 & 
                              goldPARR_alarms$location == current_location)
    
    # Assign cluster number
    cluster_numbers[within_30_days] <- cluster_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 &
                                  goldPARR_alarms$location == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same cluster number to the next alarm
      cluster_numbers[next_alarm_index] <- cluster_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB[next_alarm_index[1]]
    }
    
    # Increment cluster number for the next cluster
    cluster_num <- cluster_num + 1
  }
}

# Add cluster number to the data frame
goldPARR_alarms$cluster_number <- as.character(cluster_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(cluster_number2 = toString(ifelse(!is.na(location), cluster_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each cluster
cluster_ranges <- goldPARR_alarms %>%
  group_by(cluster_number, location) %>%
  summarize(start_date_cluster = min(week_end), end_date_cluster = max(week_end))
cluster_ranges <- subset(cluster_ranges, !is.na(cluster_ranges$cluster_number))
# Merge cluster_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, cluster_ranges, by = c("cluster_number"), keep = FALSE)
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(start_date_cluster), "ABSENT"))) %>%
  mutate(end_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(end_date_cluster), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( cluster_ranges, cluster_num, cluster_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below the alarm number and alarm ranges are added. This is done per location
# Note that the count that caused the alarm is different from the number of patients belonging to a alarm.
# A alarm is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store alarm numbers
alarm_numbers <- rep(NA, nrow(goldPARR_alarms))
alarm_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB[i]
  current_location <- goldPARR_alarms$location.x[i]
  
  # Check if alarm number is already assigned within 30 days of the current row's date and same location
  if (is.na(alarm_numbers[i])) {
    # Find rows within a 30-day window previous to the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB >= current_date - 30 & 
                               goldPARR_alarms$AfnameDatum_LAB <= current_date &
                              goldPARR_alarms$location.x == current_location)
    
    # Assign alarm number
    alarm_numbers[within_30_days] <- alarm_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 &
                                  goldPARR_alarms$location.x == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same alarm number to the next alarm
      alarm_numbers[next_alarm_index] <- alarm_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB[next_alarm_index[1]]
    }
    
    # Increment alarm number for the next alarm
    alarm_num <- alarm_num + 1
  }
}

# Add alarm number to the data frame
goldPARR_alarms$alarm_number <- as.character(alarm_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(alarm_number2 = toString(ifelse(!is.na(location.x), alarm_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each alarm
alarm_ranges <- goldPARR_alarms %>%
  group_by(alarm_number, location.x) %>%
  summarize(start_date_alarms = min(week_end), end_date_alarms = max(week_end))
alarm_ranges <- subset(alarm_ranges, !is.na(alarm_ranges$alarm_number))
# Merge alarm_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, alarm_ranges, by = c("alarm_number"), keep = FALSE)
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(start_date_alarms), "ABSENT"))) %>%
  mutate(end_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(end_date_alarms), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( alarm_ranges, alarm_num, alarm_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below only the information of the Tuesdays is kept (because that is the day that the system is runned).
# All rows are splitted again, because all information was now in one row.
# Alarms that were raised despite a cluster size < 2 will be turned off.

# Filter rows where day_of_week is Tuesday
goldPARR_small <- goldPARR_alarms %>%
  filter(day_of_week == "Tuesday")

# Convert 'week_end' to week numbers
goldPARR_small <- goldPARR_small %>%
  mutate(week_number = week(AfnameDatum_LAB))

# Remove the "ABSENT" from the strings to keep only the information needed
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "ABSENT, |ABSENT$", "")

goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "ABSENT, |ABSENT$", "")

# Split the location_per_week column into separate rows
goldPARR_small_2 <- goldPARR_small %>%
  mutate(
    location_per_week = gsub("^\\s+|\\s+$", "", location_per_week),
    studyId_outbreakdetec_Patient_per_week = gsub("^\\s+|\\s+$", "", studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = gsub("^\\s+|\\s+$", "", AfnameDatum_LAB_per_week),
    Alarm_per_week2 = gsub("^\\s+|\\s+$", "", Alarm_per_week2),
    count_per_week2 = gsub("^\\s+|\\s+$", "", count_per_week2),
    count_per_month2 = gsub("^\\s+|\\s+$", "", count_per_month2),
    count_per_30days2 = gsub("^\\s+|\\s+$", "", count_per_30days2),
    cluster_number2 = gsub("^\\s+|\\s+$", "", cluster_number2),
    alarm_number2 = gsub("^\\s+|\\s+$", "", alarm_number2),
    start_date_cluster2 = gsub("^\\s+|\\s+$", "", start_date_cluster2),
    end_date_cluster2 = gsub("^\\s+|\\s+$", "", end_date_cluster2),
    start_date_alarms2 = gsub("^\\s+|\\s+$", "", start_date_alarms2),
    end_date_alarms2 = gsub("^\\s+|\\s+$", "", end_date_alarms2)
  ) %>%
  separate_rows(location_per_week, studyId_outbreakdetec_Patient_per_week, , AfnameDatum_LAB_per_week, Alarm_per_week2, count_per_week2, count_per_month2, count_per_30days2, cluster_number2, alarm_number2, start_date_cluster2, end_date_cluster2, start_date_alarms2, end_date_alarms2, sep = ",") %>%
  mutate(
    location_per_week = str_trim(location_per_week),
    studyId_outbreakdetec_Patient_per_week = str_trim(studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = str_trim(AfnameDatum_LAB_per_week),
    Alarm_per_week2 = str_trim(Alarm_per_week2),
    count_per_week2 = as.integer(str_trim(count_per_week2)),
    count_per_month2 = as.integer(str_trim(count_per_month2)),
    count_per_30days2 = as.integer(str_trim(count_per_30days2)),
    cluster_number2 = str_trim(cluster_number2),
    alarm_number2 = str_trim(alarm_number2),
    start_date_cluster2 = str_trim(start_date_cluster2),
    end_date_cluster2 = str_trim(end_date_cluster2),
    start_date_alarms2 = str_trim(start_date_alarms2),
    end_date_alarms2 = str_trim(end_date_alarms2)
)

# Change NA to "NA"
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_cluster2 = na_if(start_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_cluster2 = na_if(end_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_alarms2 = na_if(start_date_alarms2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_alarms2 = na_if(end_date_alarms2, "NA"))

# Format to a Date
goldPARR_small_2$start_date_cluster2 <- as.Date(goldPARR_small_2$start_date_cluster2)
goldPARR_small_2$end_date_cluster2 <- as.Date(goldPARR_small_2$end_date_cluster2)
goldPARR_small_2$start_date_alarms2 <- as.Date(goldPARR_small_2$start_date_alarms2)
goldPARR_small_2$end_date_alarms2 <- as.Date(goldPARR_small_2$end_date_alarms2)
goldPARR_small_2$AfnameDatum_LAB_per_week <- as.Date(goldPARR_small_2$AfnameDatum_LAB_per_week)

# an alarm should only be raised if cluster_size is 2 or bigger
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    cluster_number2 = as.character(cluster_number2) # Ensure cluster_number2 is character
  ) %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2 
      )) %>%
  ungroup() %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(cluster_number2 = if_else(cluster_number2 %in% cluster_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, cluster_number2)) %>%
  ungroup()

# do also for the alarm number
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    alarm_number2 = as.character(alarm_number2) # Ensure alarm_number2 is character
  ) %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2 
      )) %>%
  ungroup() %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(alarm_number2 = if_else(alarm_number2 %in% alarm_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, alarm_number2)) %>%
  ungroup()

goldPARR_small_2 <- goldPARR_small_2 %>% mutate(cluster_number2 = na_if(cluster_number2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(alarm_number2 = na_if(alarm_number2, "NA"))

# Split rows belonging to two patients
goldPARR_small_2 <- goldPARR_small_2 %>%
  separate_rows(studyId_outbreakdetec_Patient_per_week, sep = "_") 

# keep only the locations of interest
goldPARR_small_2 <- goldPARR_small_2 %>%
  filter(location_per_week == specific_location)

goldPARR_alarms_basecaseWARDS <- goldPARR_small_2
```

``` r
# The script below creates an heatmap to show the results for 2019

# Clean and process data
goldPARR_small_2_clean <- goldPARR_small_2 %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB, "%m"))),
         year = format(AfnameDatum_LAB, "%Y"),
         year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  group_by(year_month, location_per_week, Alarm_per_week2) %>%
  summarize(count_per_30days2 = sum(count_per_30days2), .groups = 'drop')

# Create a dataset with gaps for each year
years <- unique(format(goldPARR_small_2_clean$year_month, "%Y"))
gaps <- data.frame(
  year_month = as.Date(paste0(years, "-12-31")),
  location_per_week = rep(unique(goldPARR_small_2_clean$location_per_week), each = length(years)),
  Alarm_per_week2 = NA,
  count_per_30days2 = NA
)

# Combine the original data with the gaps
goldPARR_small_2_with_gaps <- bind_rows(goldPARR_small_2_clean, gaps) %>%
  arrange(year_month, location_per_week)

# Plot using ggplot2
goldPARR_plot_heatmap <- ggplot(goldPARR_small_2_with_gaps, aes(x = year_month, y = location_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 30, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Date", y = "Location", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = c(0, 0))

print(goldPARR_plot_heatmap)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/Plot%20goldPA%20per%20department-1.png)<!-- -->

``` r
# clean environment
rm( aligned_plots, goldPARR_plot_heatmap, goldPA, goldPA.P75 )
```

**Fig. 2** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(base case ICU cluster). Red boxes indicate the month where the P75
threshold was crossed, green boxes are values below or equal to the
threshold. A grey box means that no threshold was available, possibly
because that ward was closed in the previous year, or no incidences
occurred in the previous year. Numbers in the boxes indicate the number
of positive tests.

  

#### P75 system - Thresholds per cluster (based on Louvain clustering)

  

See also script “Patient movements_Louvain clustering_02072024”. Louvain
clustering was performed on the UMCU and WKZ network separately.
Clusters from 2019 were used. Instead of calculating the threshold per
ward, now the group of wards is used.

  

``` r
# Remove duplicated rows
# Keep samples that are more than 1 year apart
goldPA <- gold_select %>% 
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 |
    as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365
  )

goldPA <- goldPA %>%
  mutate(year = as.character(year))

# Merge goldPA with Louvain.cluster.results
goldPA <- goldPA %>%
  left_join(Louvain.cluster.results, by = c("location" = "location", "year" = "Year"))

# If no match is found, assign "Cluster NA"
goldPA <- goldPA %>%
  mutate(Louvain_cluster = ifelse(is.na(Louvain_cluster), "Cluster NA", Louvain_cluster))
```

``` r
# Below we add the count per month, week and 30 days to the dataset 
# Also rows of multiple patients (in the same location) that has the same AfnameDatum are merged together

# Convert AfnameDatum_LAB to a proper date format
goldPA$AfnameDatum_LAB <- as.Date(goldPA$AfnameDatum_LAB)
goldPA <- goldPA[order(goldPA$AfnameDatum_LAB),]

# Group by location and date, then calculate the cumulative count (per month and per week)
goldPAR <- goldPA %>%
  arrange(AfnameDatum_LAB) %>%
  group_by(Louvain_cluster, month = format(AfnameDatum_LAB, "%Y-%m")) %>%
  mutate(count_per_month = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB, Louvain_cluster, location, count_per_month)

goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB) %>%
  mutate(week = format(AfnameDatum_LAB - lubridate::wday(AfnameDatum_LAB, week_start = 3) + 1, "%Y-%U")) %>% # per month: month = format(AfnameDatum_LAB, "%Y-%m")
  group_by(Louvain_cluster, week) %>% 
  mutate(count_per_week = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB, Louvain_cluster, location, count_per_month, count_per_week)

# First calculate the dates from the first next Tuesday and the previous 30 days
goldPAR <- goldPAR %>%
  mutate(next_tuesday = AfnameDatum_LAB + lubridate::days((9 - (lubridate::wday(AfnameDatum_LAB) - 1) %% 7) %% 7),
         thirty_days_ago = next_tuesday - lubridate::days(30))

# Calculate count_per_30days
goldPAR2 <- goldPAR %>%
  left_join(goldPAR, by = "Louvain_cluster") %>%
  group_by(Louvain_cluster, studyId_outbreakdetec_Patient.x, AfnameDatum_LAB.x) %>%
  summarize(count_per_30days = sum(AfnameDatum_LAB.y >= thirty_days_ago.x & AfnameDatum_LAB.y <= next_tuesday.x),
            .groups = "drop")

# Change colnames
colnames(goldPAR2) <- c("Louvain_cluster", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB", "count_per_30days")

# Add the counts from the 30 previous days
goldPARR <- merge(goldPAR, goldPAR2[, c("Louvain_cluster", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB", "count_per_30days")],
                   by = c("Louvain_cluster", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB"),
                   all.x = TRUE) %>% 
  arrange(AfnameDatum_LAB, studyId_outbreakdetec_Patient)
 
# Merge rows with the same AfnameDatum_LAB
# Patients IDs are combined, separated by a _, the max count is used for that row
goldPARR <- goldPARR %>%
  group_by( AfnameDatum_LAB, Louvain_cluster ) %>%
  summarize( studyId_outbreakdetec_Patient = paste( unique( studyId_outbreakdetec_Patient ), collapse = '_' ),                 
    location = location,
             count_per_month = max( count_per_month ),
             count_per_week = max( count_per_week ),
             count_per_30days = max( count_per_30days ))

# Add columns for month and year
goldPARR$month <- format(goldPARR$AfnameDatum_LAB, "%m")
goldPARR$year <- format(goldPARR$AfnameDatum_LAB, "%Y")

# clean environment
rm( goldPA, goldPAR, goldPAR2 )
```

The P75 threshold is calculated based on the counts (calculated per 30
days) per Louvain cluster of the previous year.

``` r
# This script calculates the thresholds per year per location

# Add the weeknumber to the dataset
goldPARR <- goldPARR %>%
  mutate(week_number = week(AfnameDatum_LAB))

# Calculate the max. count per week/year/cluster
goldPARR2 <- goldPARR %>%
  group_by(week_number, year, month, Louvain_cluster) %>%
  summarize(count_per_30days = max(count_per_30days))

# Calculate the threshold per year and cluster
goldPA.P75 <- goldPARR2 %>%
  group_by( year, Louvain_cluster ) %>%
  summarize( Quantile_75 = quantile( count_per_30days, probs = 0.75 )) 

# Add locations, before calculating the PrevYear threshold, because they are different per year
location_summary <- Louvain.cluster.results %>%
  group_by(Louvain_cluster) %>%
  summarize(location = unique(location))
goldPA.P75 <- goldPA.P75 %>%
  left_join(location_summary, by = c("Louvain_cluster")) %>%
  # Separate locations into individual rows
  mutate(location = str_split(location, ", ")) %>%
  unnest(location)

# Create a new column for the previous year's p75 value
goldPA.P75 <- goldPA.P75 %>%
  arrange(location, year) %>%
  group_by(location) %>%
  mutate(PrevYear_P75 = dplyr::lag(Quantile_75))

# Because sometimes the location was in another cluster the previous year use the median threshold per year and cluster
goldPA.P75 <- goldPA.P75 %>%
  group_by(year, Louvain_cluster) %>%
  mutate(PrevYear_P75_median = median(PrevYear_P75, na.rm = TRUE)) %>%
  ungroup()

# Sort the data
goldPA.P75 <- goldPA.P75 %>% arrange( year, Louvain_cluster, location )

# clean environment
rm( goldPARR2 )
```

``` r
# The script below adds the alarms to the data
# Because we simulate that the system is used every Tuesday all information is moved to that Tuesday.

# Generate a sequence of dates from January 2014 to December 2021
date_sequence <- seq( as.Date( "2014-01-01" ), as.Date( "2021-12-31" ), by = "day" )
# Create a dataframe with all possible dates
all_dates <- data.frame( AfnameDatum_LAB = date_sequence )
# Merge the filtered dataframe with all_dates, filling missing values with 0
goldPARR_merged <- merge( all_dates, goldPARR, by = "AfnameDatum_LAB", all.x = TRUE )
goldPARR_merged$count_per_30days[ is.na( goldPARR_merged$count_per_30days )] <- 0
goldPARR_merged$count_per_week[ is.na( goldPARR_merged$count_per_week )] <- 0
goldPARR_merged$count_per_month[ is.na( goldPARR_merged$count_per_month )] <- 0

# Fill in the new rows for month and year
goldPARR_merged$month <- format(goldPARR_merged$AfnameDatum_LAB, "%m")
goldPARR_merged$year <- format(goldPARR_merged$AfnameDatum_LAB, "%Y")
# Merge with goldPA.P75 to add PrevYear_P75 for each year
goldPARR_merged <- goldPARR_merged %>% left_join(goldPA.P75, by = c("year", "Louvain_cluster", "location"))
# Create a new column 'Above_Quantile' to represent whether count is above PrevYear_P75
goldPARR_merged$Above_Quantile <- goldPARR_merged$count_per_30days > goldPARR_merged$PrevYear_P75_median
# Remove the column with the threshold of that year (we use the one of the previous year)
goldPARR_merged <- select(goldPARR_merged, -c("Quantile_75", "PrevYear_P75" ))

# Create a new dataframe and add weekdays
goldPARR_alarms <- goldPARR_merged
goldPARR_alarms$day_of_week <- weekdays(goldPARR_alarms$AfnameDatum_LAB)
goldPARR_alarms$day_of_week <- factor(goldPARR_alarms$day_of_week, levels = c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday"))

# Group by week and check if there was any Above_Quantile == TRUE in the previous week (including that day)
# Add all the variables from a certain week on the Tuesday
goldPARR_alarms <- goldPARR_alarms %>%
  arrange(AfnameDatum_LAB) %>%
  mutate(week_end = as.Date(AfnameDatum_LAB) + 6 - (as.numeric(format(AfnameDatum_LAB, "%u")) - 3) %% 7) %>%
  group_by(week_end, Louvain_cluster) %>%
  mutate(
    Alarm_per_week = ifelse(any(Above_Quantile == TRUE) & !all(Above_Quantile == FALSE | is.na(Above_Quantile)), TRUE, 
                            ifelse(all(is.na(Above_Quantile)), NA, FALSE)),
    count_per_week = max(count_per_week),
    count_per_month = max(count_per_month),
    count_per_30days = max(count_per_30days)
  ) %>%
    ungroup() %>%
  group_by(week_end) %>%
  mutate(
    Louvain_cluster_per_week = toString(na.omit(Louvain_cluster)),
    location_per_week = toString(na.omit(location)),
    studyId_outbreakdetec_Patient_per_week = toString(na.omit(studyId_outbreakdetec_Patient)),
    AfnameDatum_LAB_per_week = toString(ifelse(!is.na(location), as.character(AfnameDatum_LAB), "ABSENT")),
    Alarm_per_week2 = toString(ifelse(!is.na(Louvain_cluster), Alarm_per_week, "ABSENT")),
    count_per_week2 = toString(ifelse(count_per_week != 0, count_per_week, NA)),
    count_per_month2 = toString(ifelse(count_per_month != 0, count_per_month, NA)),
    count_per_30days2 = toString(ifelse(count_per_30days != 0, count_per_30days, NA))
  ) %>%
  mutate(
    count_per_week2 = gsub(", NA", "", count_per_week2),
    count_per_month2 = gsub(", NA", "", count_per_month2),
    count_per_30days2 = gsub(", NA", "", count_per_30days2),
    count_per_week2 = gsub("NA, ", "", count_per_week2),
    count_per_month2 = gsub("NA, ", "", count_per_month2),
    count_per_30days2 = gsub("NA, ", "", count_per_30days2),
    count_per_week2 = gsub("NA", "", count_per_week2),
    count_per_month2 = gsub("NA", "", count_per_month2),
    count_per_30days2 = gsub("NA", "", count_per_30days2)
  )

# clean environment
rm( date_sequence, all_dates, goldPARR, goldPARR_merged )
```

``` r
# Below the cluster number and cluster ranges are added. This is done per cluster
# Note that the count that caused the alarm is different from the number of patients belonging to a cluster.
# A cluster is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store cluster numbers
cluster_numbers <- rep(NA, nrow(goldPARR_alarms))
cluster_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and Louvain_cluster
  current_date <- goldPARR_alarms$AfnameDatum_LAB[i]
  current_Louvain_cluster <- goldPARR_alarms$Louvain_cluster[i]
  
  # Check if cluster number is already assigned within 30 days of the current row's date and same Louvain_cluster
  if (is.na(cluster_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same Louvain_cluster
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 & 
                              goldPARR_alarms$Louvain_cluster == current_Louvain_cluster)
    
    # Assign cluster number
    cluster_numbers[within_30_days] <- cluster_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which( goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 &
                                  goldPARR_alarms$Louvain_cluster == current_Louvain_cluster)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same cluster number to the next alarm
      cluster_numbers[next_alarm_index] <- cluster_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB[next_alarm_index[1]]
    }
    
    # Increment cluster number for the next cluster
    cluster_num <- cluster_num + 1
  }
}

# Add cluster number to the data frame
goldPARR_alarms$cluster_number <- as.character(cluster_numbers)

# Expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, Louvain_cluster_per_week) %>%
  mutate(cluster_number2 = toString(ifelse(!is.na(location), cluster_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each cluster
cluster_ranges <- goldPARR_alarms %>%
  group_by(cluster_number, Louvain_cluster) %>%
  summarize(start_date_cluster = min(week_end), end_date_cluster = max(week_end))
cluster_ranges <- subset(cluster_ranges, !is.na(cluster_ranges$cluster_number))
# Merge cluster_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, cluster_ranges, by = c("cluster_number"))
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_cluster2 = toString(ifelse(!is.na(Louvain_cluster.x), as.character(start_date_cluster), "ABSENT"))) %>%
  mutate(end_date_cluster2 = toString(ifelse(!is.na(Louvain_cluster.x), as.character(end_date_cluster), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( cluster_ranges, cluster_num, cluster_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below the alarm number and alarm ranges are added. This is done per Louvain_cluster
# Note that the count that caused the alarm is different from the number of patients belonging to a alarm.
# A alarm is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store alarm numbers
alarm_numbers <- rep(NA, nrow(goldPARR_alarms))
alarm_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and Louvain_cluster
  current_date <- goldPARR_alarms$AfnameDatum_LAB[i]
  current_location <- goldPARR_alarms$Louvain_cluster.x[i]
  
  # Check if alarm number is already assigned within 30 days of the current row's date and same Louvain_cluster
  if (is.na(alarm_numbers[i])) {
    # Find rows within a 30-day window previous to the current row's date and same Louvain_cluster
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB <= current_date &
                              goldPARR_alarms$Louvain_cluster.x == current_location)
    
    # Assign alarm number
    alarm_numbers[within_30_days] <- alarm_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB <= current_date + 30 &
                                  goldPARR_alarms$Louvain_cluster.x == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same alarm number to the next alarm
      alarm_numbers[next_alarm_index] <- alarm_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB[next_alarm_index[1]]
    }
    
    # Increment alarm number for the next alarm
    alarm_num <- alarm_num + 1
  }
}

# Add alarm number to the data frame
goldPARR_alarms$alarm_number <- as.character(alarm_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(alarm_number2 = toString(ifelse(!is.na(location), alarm_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each alarm
alarm_ranges <- goldPARR_alarms %>%
  group_by(alarm_number, Louvain_cluster.x) %>%
  summarize(start_date_alarms = min(week_end), end_date_alarms = max(week_end))
alarm_ranges <- subset(alarm_ranges, !is.na(alarm_ranges$alarm_number))
# Merge alarm_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, alarm_ranges, by = c("alarm_number"), keep = FALSE)
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_alarms2 = toString(ifelse(!is.na(Louvain_cluster.x.x), as.character(start_date_alarms), "ABSENT"))) %>%
  mutate(end_date_alarms2 = toString(ifelse(!is.na(Louvain_cluster.x.x), as.character(end_date_alarms), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( alarm_ranges, alarm_num, alarm_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below only the information of the Tuesdays is kept (because that is the day that the system is runned).
# all rows are splitted again, because all information was now in one row.
# Alarms that were raised despite a cluster size < 2 will be turned off.

# Filter rows where day_of_week is Tuesday
goldPARR_small <- goldPARR_alarms %>%
  filter(day_of_week == "Tuesday")

# Convert 'week_end' to week numbers
goldPARR_small <- goldPARR_small %>%
  mutate(week_number = week(AfnameDatum_LAB))

goldPARR_small$AfnameDatum_LAB <- as.Date(goldPARR_small$AfnameDatum_LAB)

# Remove the "ABSENT" from the strings to keep only the information needed
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "ABSENT, |ABSENT$", "")

goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "ABSENT, |ABSENT$", "")

# Split the location_per_week column into separate rows
goldPARR_small_2 <- goldPARR_small %>%
  mutate(
    Louvain_cluster_per_week = gsub("^\\s+|\\s+$", "", Louvain_cluster_per_week),
    location_per_week = gsub("^\\s+|\\s+$", "", location_per_week),
    studyId_outbreakdetec_Patient_per_week = gsub("^\\s+|\\s+$", "", studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = gsub("^\\s+|\\s+$", "", AfnameDatum_LAB_per_week),
    Alarm_per_week2 = gsub("^\\s+|\\s+$", "", Alarm_per_week2),
    count_per_week2 = gsub("^\\s+|\\s+$", "", count_per_week2),
    count_per_month2 = gsub("^\\s+|\\s+$", "", count_per_month2),
    count_per_30days2 = gsub("^\\s+|\\s+$", "", count_per_30days2),
    cluster_number2 = gsub("^\\s+|\\s+$", "", cluster_number2),
    alarm_number2 = gsub("^\\s+|\\s+$", "", alarm_number2),
    start_date_cluster2 = gsub("^\\s+|\\s+$", "", start_date_cluster2),
    end_date_cluster2 = gsub("^\\s+|\\s+$", "", end_date_cluster2),
    start_date_alarms2 = gsub("^\\s+|\\s+$", "", start_date_alarms2),
    end_date_alarms2 = gsub("^\\s+|\\s+$", "", end_date_alarms2)
  ) %>%
  separate_rows(Louvain_cluster_per_week, location_per_week, studyId_outbreakdetec_Patient_per_week, , AfnameDatum_LAB_per_week, Alarm_per_week2, count_per_week2, count_per_month2, count_per_30days2, cluster_number2, alarm_number2, start_date_cluster2, end_date_cluster2, start_date_alarms2, end_date_alarms2, sep = ",") %>%
  mutate(
     Louvain_cluster_per_week = str_trim(Louvain_cluster_per_week),
     location_per_week = str_trim(location_per_week),
     studyId_outbreakdetec_Patient_per_week = str_trim(studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = str_trim(AfnameDatum_LAB_per_week),
    Alarm_per_week2 = str_trim(Alarm_per_week2),
    count_per_week2 = as.integer(str_trim(count_per_week2)),
    count_per_month2 = as.integer(str_trim(count_per_month2)),
    count_per_30days2 = as.integer(str_trim(count_per_30days2)),
    cluster_number2 = str_trim(cluster_number2),
    alarm_number2 = str_trim(alarm_number2),
    start_date_cluster2 = str_trim(start_date_cluster2),
    end_date_cluster2 = str_trim(end_date_cluster2),
    start_date_alarms2 = str_trim(start_date_alarms2),
    end_date_alarms2 = str_trim(end_date_alarms2)
)

# Change NA to "NA"
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_cluster2 = na_if(start_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_cluster2 = na_if(end_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_alarms2 = na_if(start_date_alarms2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_alarms2 = na_if(end_date_alarms2, "NA"))

# Convert to a Date
goldPARR_small_2$start_date_cluster2 <- as.Date(goldPARR_small_2$start_date_cluster2)
goldPARR_small_2$end_date_cluster2 <- as.Date(goldPARR_small_2$end_date_cluster2)
goldPARR_small_2$start_date_alarms2 <- as.Date(goldPARR_small_2$start_date_alarms2)
goldPARR_small_2$end_date_alarms2 <- as.Date(goldPARR_small_2$end_date_alarms2)
goldPARR_small_2$AfnameDatum_LAB_per_week <- as.Date(goldPARR_small_2$AfnameDatum_LAB_per_week)

# An alarm should only be raised if cluster_size is 2 or bigger
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    cluster_number2 = as.character(cluster_number2) # Ensure cluster_number2 is character
  ) %>%
  group_by(cluster_number2, Louvain_cluster_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2
    )
) %>%
  ungroup() %>%
  group_by(cluster_number2, Louvain_cluster_per_week) %>%
  mutate(cluster_number2 = if_else(cluster_number2 %in% cluster_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, cluster_number2)) %>%
  ungroup()

# do also for the alarm number
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    alarm_number2 = as.character(alarm_number2) # Ensure alarm_number2 is character
  ) %>%
  group_by(alarm_number2, Louvain_cluster_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2
    )
) %>%
  ungroup() %>%
  group_by(alarm_number2, Louvain_cluster_per_week) %>%
  mutate(alarm_number2 = if_else(alarm_number2 %in% alarm_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, alarm_number2)) %>%
  ungroup()

goldPARR_small_2 <- goldPARR_small_2 %>% mutate(cluster_number2 = na_if(cluster_number2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(alarm_number2 = na_if(alarm_number2, "NA"))

# Split rows belonging to two patients
goldPARR_small_2 <- goldPARR_small_2 %>%
  separate_rows(studyId_outbreakdetec_Patient_per_week, sep = "_") 

# keep only the locations of interest
goldPARR_small_2 <- goldPARR_small_2 %>%
  filter(Louvain_cluster_per_week == specific_cluster)

goldPARR_alarms_clusters <- goldPARR_small_2
```

``` r
# The script below creates a heatmap to show the results for 2019

# Clean and process data
goldPARR_small_2_clean <- goldPARR_small_2 %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB, "%m"))),
         year = format(AfnameDatum_LAB, "%Y"),
         year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(Louvain_cluster_per_week = ifelse(Louvain_cluster_per_week == "", NA, Louvain_cluster_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(Louvain_cluster_per_week)) %>%  # Exclude rows where Louvain_cluster_per_week is NA
  filter(Louvain_cluster_per_week != "Cluster NA") %>% 
  group_by(year_month, Louvain_cluster_per_week, Alarm_per_week2) %>%
  summarize(count_per_30days2 = sum(count_per_30days2), .groups = 'drop')

# Create a dataset with gaps for each year
years <- unique(format(goldPARR_small_2_clean$year_month, "%Y"))
gaps <- data.frame(
  year_month = as.Date(paste0(years, "-12-31")),
  Louvain_cluster_per_week = rep(unique(goldPARR_small_2_clean$Louvain_cluster_per_week), each = length(years)),
  Alarm_per_week2 = NA,
  count_per_30days2 = NA
)

# Combine the original data with the gaps
goldPARR_small_2_with_gaps <- bind_rows(goldPARR_small_2_clean, gaps) %>%
  arrange(year_month, Louvain_cluster_per_week)

# Define the desired order for Louvain_cluster_per_week
desired_order <- unique(Louvain.cluster.results$Louvain_cluster) %>% rev()

# Plot using ggplot2
goldPARR_plot_heatmap <- ggplot(goldPARR_small_2_with_gaps, aes(x = year_month, y = Louvain_cluster_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 30, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Date", y = "Location", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = c(0, 0)) +
  scale_y_discrete(limits = desired_order)

print(goldPARR_plot_heatmap)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/Plot%20goldPA%20per%20louvain%20cluster-1.png)<!-- -->

``` r
# clean environment
rm( aligned_plots, goldPARR_plot_heatmap, goldPARR_alarms, goldPA )
```

**Fig. 3** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(Louvain clustering for ICU cluster). Red boxes indicate the month where
the P75 threshold was crossed, green boxes are values below or equal to
the threshold. A grey box means that no threshold was available,
possibly because that ward was closed in the previous year, or no
incidences occurred in the previous year. Numbers in the boxes indicate
the number of positive tests.

``` r
# The script below creates an heatmap to show the results for 2019

# Clean and process data
goldPARR_small_2_clean <- goldPARR_small_2 %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB, "%m"))),
         year = format(AfnameDatum_LAB, "%Y"),
         year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  group_by(year_month, location_per_week, Louvain_cluster_per_week, Alarm_per_week2) %>%
  summarize(count_per_30days2 = sum(count_per_30days2), .groups = 'drop')

# Create a dataset with gaps for each year
years <- unique(format(goldPARR_small_2_clean$year_month, "%Y"))
gaps <- data.frame(
  year_month = as.Date(paste0(years, "-12-31")),
  location_per_week = rep(unique(goldPARR_small_2_clean$location_per_week), each = length(years)),
  Alarm_per_week2 = NA,
  count_per_30days2 = NA
)

# Combine the original data with the gaps
goldPARR_small_2_with_gaps <- bind_rows(goldPARR_small_2_clean, gaps) %>%
  arrange(year_month, location_per_week)

# Define the desired order for location_per_week
desired_order <- goldPA.P75 %>%
  filter(year == 2019) %>%
  distinct(location) %>%
  pull(location) %>%
  rev()

# Plot using ggplot2
goldPARR_plot_heatmap <- ggplot(goldPARR_small_2_with_gaps, aes(x = year_month, y = location_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 30, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Date", y = "Location", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = c(0, 0)) +
  scale_y_discrete(limits = desired_order)

print(goldPARR_plot_heatmap)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/Plot%20goldPA%20per%20louvain%20cluster%202-1.png)<!-- -->

``` r
# clean environment
rm( gaps, goldPA_march, goldPA.P75, goldPARR_small, goldPARR_small_2, goldPARR_small_2_clean, goldPARR_small_2_with_gaps, location_summary, current_Louvain_cluster, desired_order, years )
```

**Fig. 4** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(Louvain clustering for ICU cluster per department). Red boxes indicate
the month where the P75 threshold was crossed, green boxes are values
below or equal to the threshold. A grey box means that no threshold was
available, possibly because that ward was closed in the previous year,
or no incidences occurred in the previous year. Numbers in the boxes
indicate the number of positive tests.

  

#### Combine base case and Louvain cluster in one plot

``` r
# goldPARR_alarms2019_basecaseWARDS
# goldPARR_alarms2019_clusters

# Filter the dataset for locations in the cluster
goldPARR_alarms_basecase_filt <- goldPARR_alarms_basecaseWARDS[is.na(goldPARR_alarms_basecaseWARDS$location_per_week) | goldPARR_alarms_basecaseWARDS$location_per_week %in% c("B531", "B430", "B330", "B340", "U440"), ]

goldPARR_alarms_basecase_filt <- goldPARR_alarms_basecase_filt %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB, "%m"))),
         year = format(AfnameDatum_LAB, "%Y"),
         year_week = as.Date(paste(year, week_number, "1", sep="-"), format="%Y-%U-%u")) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  filter(year == "2019") 

goldPARR_alarms_clusters_filt <- goldPARR_alarms_clusters[is.na(goldPARR_alarms_clusters$location_per_week) | goldPARR_alarms_clusters$location_per_week %in% c("B531", "B430", "B330", "B340", "U440"), ]

goldPARR_alarms_clusters_filt <- goldPARR_alarms_clusters_filt %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB, "%m"))),
         year = format(AfnameDatum_LAB, "%Y"),
         year_week = as.Date(paste(year, week_number, "1", sep="-"), format="%Y-%U-%u")) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  filter(year == "2019") 


# Create the first plot
plot1 <- ggplot(goldPARR_alarms_basecase_filt, aes(x = week_number, y = location_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 0.9, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = NULL, y = "Location", fill = "Above Quantile") +  # Remove x-axis label
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top",
    legend.direction = "vertical"  # Set legend direction to vertical
  ) +
  scale_x_discrete(limits = as.character(1:52))

# Create the second plot
plot2 <- ggplot(goldPARR_alarms_clusters_filt, aes(x = week_number, y = Louvain_cluster_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 0.9, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Weeknumber 2019", y = "Cluster", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "none"  # Remove legend from the second plot
  ) +
  scale_x_discrete(limits = as.character(1:52)) 

# Combine the plots using patchwork
combined_plot <- plot1 / plot2 + plot_layout(guides = "collect")

# Print the combined plot
print(combined_plot)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/goldPA%20compare%20base%20case%20and%20clusters-1.png)<!-- -->

``` r
file_path <- file.path("~/Documents/AODS_Susanne/Patient_movements/Results/Results patient movements", selected_species, specific_cluster, "P75/Descriptive results/combined_plot2.png")

# Save the combined plot
#ggsave(file_path, combined_plot, width = 14, height = 7)

# clean environment
rm( combined_plot, plot1, plot2, goldPARR_alarms_basecase_filt, goldPARR_alarms_clusters_filt )
```

**Fig. 5** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(per ward (upper) and cluster of wards (lower) for ICU cluster). Red
boxes indicate the month where the P75 threshold was crossed, green
boxes are values below or equal to the threshold. A grey box means that
no threshold was available, possibly because that ward was closed in the
previous year, or no incidences occurred in the previous year. Numbers
in the boxes indicate the number of positive tests.

  

#### P75 system - Thresholds including patient history (one point per location)

  

Now, every location the patient has visited in the 14 days preceding the
positive test will be counted.

  

``` r
# Remove duplicated rows
# Keep samples that are more than 1 year apart
goldPA <- gold_select %>% 
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 |
    as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365
  )
```

``` r
# The script below prepares the movement data and laboratory data for merging them.

# Rename columns in Patient_movements4_merged
Patient_movements1.rename <- Patient_movements4_merged %>%
  rename(
    #age_at_lab = age_index,
    Startdatum_OPNMUT = hos_mut_start_dt,
    Einddatum_OPNMUT = hos_mut_stop_dt,
    location = hos_mut_Afd_code,
    AfdelingOmschrijving_OPNMUT = hos_mut_afd_Omschrijving,
    Opnamedatum_OPN = hos_start_dt,
    Ontslagdatum_OPN = hos_stop_dt
  )

# Columns to be removed
columns_to_remove <- c(
  "umc_bezwaar_dt", "research2", "edu2", "recontact2", "data2", "bezwaar3", "bezwaar4", "recontract3",
  "AMIKACINE", "AMOXICILLINE", "AMPHOTERICINE_B", "AMPICILLIN", "AMPICILLINE", "ANIDULAFUNGIN",
  "AUGMENTIN", "AZITHROMYCINE", "AZTREONAM", "CASPOFUNGINE", "CEFAZOLINE", "CEFEPIME", "CEFOTAXIME",
  "CEFTAROLINE", "CEFTAZIDIME", "CEFTAZIDIME-AVIBACTAM", "CEFTOLOZANE_TAZOBACTAM", "CEFTRIAXON",
  "CEFUROXIM", "CEPHALOTHIN", "CHLOORAMPHENICOL", "CIPROFLOXACIN", "CLARITHROMYCINE", "CLINDAMYCINE",
  "CLOFAZIMINE", "CO_TRIMOXAZOL", "COLISTINE_POLYMYXINE", "CYCLOSERINE", "DAPTOMYCINE", "DOXYCYCLINE",
  "ERTAPENEM", "ERYTROMYCINE", "ESBL", "ETHAMBUTOL", "FLUCLOXACILLINE", "FLUCONAZOLE", "FLUCYTOSINE",
  "FOSFOMYCINE", "FUSIDINEZUUR", "GENTAMICINE", "HIGH_LEVEL_GENTAMICINE", "IMIPENEM", "INH.x",
  "ISAVUCONAZOL", "ITRACONAZOLE", "KETOCONAZOLE", "LEVOFLOXACIN", "LINEZOLID", "MEROPENEM", "METRONIDAZOLE",
  "MICAFUNGIN", "MICONAZOLE", "MINOCYCLINE", "MOXIFLOXACIN", "MUPIROCINE", "MUPIROCINE_H", "NEOMYCINE",
  "NITROFURANTOINE", "NORFLOXACIN", "OFLOXACIN", "OROTOMIDE", "PENICILLIN", "PENICILLINE", "PIPERA_TAZOBACTAM",
  "PIPERACILLINE", "POSACONAZOLE", "PROTIONAMIDE", "PYRAZINAMIDE", "QUINUPRISTINE", "RIFABUTINE", "RIFAMPICINE",
  "SPECTINOMYCINE", "STREPTOMYCINE", "TEICOPLANINE", "TEMOCILLINE", "TETRACYCLINE", "TIGECYCLINE", "TOBRAMYCINE",
  "TRIMETHOPRIM", "UNKOWN", "VANCOMYCINE", "VORICONAZOLE"
)

# Remove specified columns
goldPA.short <- goldPA[, !(names(goldPA) %in% columns_to_remove)]

# Ensure both dataframes have the same columns in the same order
common_columns <- union(names(goldPA.short), names(Patient_movements1.rename))
# Create an empty dataframe with all common columns and fill with NA
empty_df <- as_tibble(setNames(rep(list(NA), length(common_columns)), common_columns))

# Add missing columns with NA in goldPA.short
goldPA.new <- bind_cols(goldPA.short, empty_df %>% select(setdiff(common_columns, names(goldPA.short))))

# Add missing columns with NA in Patient_movements1.rename
Patient_movements1.new <- bind_cols(Patient_movements1.rename, empty_df %>% select(setdiff(common_columns, names(Patient_movements1.rename))))

# Order columns in the same order
goldPA.new <- goldPA.new[, common_columns]
Patient_movements1.new <- Patient_movements1.new[, common_columns]

# Merge rows for common studyId_outbreakdetec_Patient
merged_df_goldPA <- bind_rows(goldPA.new, Patient_movements1.new %>%
                         filter(studyId_outbreakdetec_Patient %in% goldPA.new$studyId_outbreakdetec_Patient))

# Format to a Date
merged_df_goldPA$Opnamedatum_OPN <- as.Date(merged_df_goldPA$Opnamedatum_OPN)
merged_df_goldPA$Ontslagdatum_OPN <- as.Date(merged_df_goldPA$Ontslagdatum_OPN)

# Sort the data
merged_df_goldPA <- merged_df_goldPA %>% arrange( studyId_outbreakdetec_Patient )
# remove the same rows (that were present in both datasets before merging)
merged_df_goldPA <- merged_df_goldPA %>%
  distinct(studyId_outbreakdetec_Patient, Startdatum_OPNMUT, Einddatum_OPNMUT, .keep_all = TRUE)

# Clean environment
rm( columns_to_remove, goldPA.short, common_columns, empty_df  )
```

``` r
# The script below keeps the rows with the unique locations withing two weeks prior to the Afnamedatum, in this way they also get a count.

# Sort the data
goldPA.new <- goldPA.new %>%
  arrange(studyId_outbreakdetec_Patient, AfnameDatum_LAB)

# Create a new dataframe with 14 rows for each date
two.weeks_complete <- goldPA.new %>%
  group_by(studyId_outbreakdetec_Patient, AfnameDatum_LAB) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  complete(studyId_outbreakdetec_Patient, row_number = 1:14) %>%
  group_by(studyId_outbreakdetec_Patient) %>%
  fill(AfnameDatum_LAB, location) %>%
  mutate(AfnameDatum_LAB_prior = AfnameDatum_LAB - (row_number - 1))
# Remove unnecessary columns
two.weeks_complete <- select(two.weeks_complete, -row_number)
two.weeks_complete <- two.weeks_complete[,c("studyId_outbreakdetec_Patient","AfnameDatum_LAB_prior")] # "location",

# Merge the dataframes based on patient ID
final_merged_df <- two.weeks_complete %>%
  full_join(merged_df_goldPA, by = "studyId_outbreakdetec_Patient") 

# Remove the rows with an Afnamedatum outside the Startdatum and Einddatum of the Opname
final_merged_df <- final_merged_df %>%
  filter(
    is.na(Einddatum_OPNMUT) |
    (AfnameDatum_LAB_prior >= Startdatum_OPNMUT & AfnameDatum_LAB_prior <= Einddatum_OPNMUT)
  ) 

# order the data
final_merged_df <- final_merged_df %>%
  arrange(studyId_outbreakdetec_Patient, desc(AfnameDatum_LAB_prior), by_group = TRUE)

# Remove duplicated rows and keep the first occurrence
final_merged_df <- distinct(final_merged_df, .keep_all = TRUE)

# Remove duplicated rows based on specified columns and keep the first occurrence
final_merged_df <- final_merged_df %>%
  distinct(studyId_outbreakdetec_Patient, AfnameDatum_LAB_prior, location, .keep_all = TRUE)

# keep the unique locations in the previous 2 weeks
final_merged_df <- final_merged_df %>%
  distinct(studyId_outbreakdetec_Patient, Opnamedatum_OPN, Ontslagdatum_OPN, location, .keep_all = TRUE)

goldPA <- final_merged_df

# Clean environment
rm( goldPA.new, two.weeks_complete, final_merged_df  )
```

``` r
# Below we add the count per month, week and 30 days to the dataset 
# Also rows of multiple patients (in the same location) that has the same AfnameDatum are merged together

# Convert AfnameDatum_LAB_prior to a proper date format
goldPA$AfnameDatum_LAB_prior <- as.Date(goldPA$AfnameDatum_LAB_prior)

# Order the data
goldPA <- goldPA[order(goldPA$studyId_outbreakdetec_Patient),]
goldPA <- goldPA[order(goldPA$AfnameDatum_LAB_prior),]

# Step 1: Filter rows where AfnameDatum_LAB is not NA
filtered_df <- goldPA %>% 
  filter(!is.na(AfnameDatum_LAB)) %>% 
  select(studyId_outbreakdetec_Patient, Opnamedatum_OPN, Ontslagdatum_OPN, AfnameDatum_LAB)
filtered_df <- filtered_df[order(filtered_df$AfnameDatum_LAB),] 
filtered_df <- distinct(filtered_df)

# Step 3: Left join the original dataframe with the lookup table to fill in missing AfnameDatum_LAB values
filled_df <- goldPA %>%
  left_join(filtered_df, by = c("studyId_outbreakdetec_Patient", "Opnamedatum_OPN", "Ontslagdatum_OPN")) 
filled_df$AfnameDatum_LAB.x[is.na(filled_df$AfnameDatum_LAB.x)] <- filled_df$AfnameDatum_LAB.y[is.na(filled_df$AfnameDatum_LAB.x)]
# Remove extra column
goldPAR <- filled_df %>%
  select(-AfnameDatum_LAB.y) %>%
  rename(AfnameDatum_LAB = AfnameDatum_LAB.x)

# Group by location and date, then calculate the cumulative count
goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB_prior) %>%
  group_by(location, month = format(AfnameDatum_LAB_prior, "%Y-%m")) %>%
  mutate(count_per_month = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB_prior, AfnameDatum_LAB, location, count_per_month)

goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB_prior) %>%
  mutate(week = format(AfnameDatum_LAB_prior - lubridate::wday(AfnameDatum_LAB_prior, week_start = 3) + 1, "%Y-%U")) %>% 
  group_by(location, week) %>% 
  mutate(count_per_week = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB_prior, AfnameDatum_LAB, location, count_per_month, count_per_week)

# First calculate the dates from the first next Tuesday and the previous 30 days
goldPAR <- goldPAR %>%
  mutate(next_tuesday = AfnameDatum_LAB_prior + lubridate::days((9 - (lubridate::wday(AfnameDatum_LAB_prior) - 1) %% 7) %% 7),
         thirty_days_ago = next_tuesday - lubridate::days(30))

goldPAR2 <- goldPAR %>%
  left_join(goldPAR, by = "location") %>%
  group_by(location, studyId_outbreakdetec_Patient.x, AfnameDatum_LAB_prior.x, AfnameDatum_LAB.x) %>%
  summarize(count_per_30days = sum(AfnameDatum_LAB_prior.y >= thirty_days_ago.x & AfnameDatum_LAB_prior.y <= next_tuesday.x),
            .groups = "drop")

# Change colnames
colnames(goldPAR2) <- c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior", "AfnameDatum_LAB","count_per_30days")

# Add the counts from the 30 previous days
goldPARR <- merge(goldPAR, goldPAR2[, c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior", "count_per_30days")],
                   by = c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior"),
                   all.x = TRUE) %>% 
  arrange(AfnameDatum_LAB_prior, studyId_outbreakdetec_Patient)
 
# Merge rows with the same AfnameDatum_LAB_prior
# Patients IDs are combined, separated by a _, the max count is used for that row
goldPARR <- goldPARR %>%
  group_by(AfnameDatum_LAB_prior, location) %>%
  summarize(AfnameDatum_LAB = AfnameDatum_LAB,
            #studyId_outbreakdetec_Patient = toString(unique(studyId_outbreakdetec_Patient)),
             studyId_outbreakdetec_Patient = paste(unique(studyId_outbreakdetec_Patient), collapse = '_'),         
            count_per_month = max(count_per_month),
             count_per_week = max(count_per_week),
             count_per_30days = max(count_per_30days))

# Add columns for month and year
goldPARR$month <- format(goldPARR$AfnameDatum_LAB_prior, "%m")
goldPARR$year <- format(goldPARR$AfnameDatum_LAB_prior, "%Y")

# clean environment
rm( filtered_df, filled_df, goldPA, goldPAR, goldPAR2 )
```

The P75 threshold is calculated based on the counts (calculated per 30
days) per location of the previous year.

``` r
# This script calculates the thresholds per year per location

# Add the weeknumber to the dataset
goldPARR <- goldPARR %>%
  mutate(week_number = week(AfnameDatum_LAB))

# Calculate the max. count per week/year/cluster
goldPARR2 <- goldPARR %>%
  group_by(week_number, year, month, location) %>%
  summarize(count_per_30days = max(count_per_30days))

# Calculate the threshold per year and cluster
goldPA.P75 <- goldPARR2 %>%
  group_by( year, location ) %>%
  summarize( Quantile_75 = quantile( count_per_30days, probs = 0.75 )) 

# Create a new column for the previous year's p75 value
goldPA.P75 <- goldPA.P75 %>%
  arrange(location, year) %>%
  group_by(location) %>%
  mutate(PrevYear_P75 = dplyr::lag(Quantile_75))

# Sort the data
goldPA.P75 <- goldPA.P75 %>% arrange( year, location )

# clean environment
rm( goldPARR2 )
```

``` r
# The script below adds the alarms to the data
# Because we simulate that the system is used every Tuesday all information is moved to that Tuesday.

# Generate a sequence of dates from January 2014 to December 2021
date_sequence <- seq( as.Date( "2014-01-01" ), as.Date( "2021-12-31" ), by = "day" )
# Create a dataframe with all possible dates
all_dates <- data.frame( AfnameDatum_LAB_prior = date_sequence )
# Merge the filtered dataframe with all_dates, filling missing values with 0
goldPARR_merged <- merge( all_dates, goldPARR, by = "AfnameDatum_LAB_prior", all.x = TRUE )
goldPARR_merged$count_per_30days[ is.na( goldPARR_merged$count_per_30days )] <- 0
goldPARR_merged$count_per_week[ is.na( goldPARR_merged$count_per_week )] <- 0
goldPARR_merged$count_per_month[ is.na( goldPARR_merged$count_per_month )] <- 0

# Fill in the new rows for month and year
goldPARR_merged$month <- format(goldPARR_merged$AfnameDatum_LAB_prior, "%m")
goldPARR_merged$year <- format(goldPARR_merged$AfnameDatum_LAB_prior, "%Y")
# Merge with goldPA.P75 to add PrevYear_P75 for each year
goldPARR_merged <- goldPARR_merged %>% left_join(goldPA.P75, by = c("year", "location"))
# Create a new column 'Above_Quantile' to represent whether count is above PrevYear_P75
goldPARR_merged$Above_Quantile <- goldPARR_merged$count_per_30days > goldPARR_merged$PrevYear_P75
# Remove the column with the threshold of that year (we use the one of the previous year)
goldPARR_merged <- select(goldPARR_merged, -c("Quantile_75" ))

# Create a new dataframe and add weekdays
goldPARR_alarms <- goldPARR_merged
goldPARR_alarms$day_of_week <- weekdays(goldPARR_alarms$AfnameDatum_LAB_prior)
goldPARR_alarms$day_of_week <- factor(goldPARR_alarms$day_of_week, levels = c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday"))

# Group by week and check if there was any Above_Quantile == TRUE in the previous week (including that day)
# Add all the variables from a certain week on the Tuesday
goldPARR_alarms <- goldPARR_alarms %>%
  arrange(AfnameDatum_LAB_prior) %>%
  mutate(week_end = as.Date(AfnameDatum_LAB_prior) + 6 - (as.numeric(format(AfnameDatum_LAB_prior, "%u")) - 3) %% 7) %>%
  group_by(week_end, location) %>%
  mutate(
    Alarm_per_week = ifelse(any(Above_Quantile == TRUE) & !all(Above_Quantile == FALSE | is.na(Above_Quantile)), TRUE, 
                            ifelse(all(is.na(Above_Quantile)), NA, FALSE)),
    count_per_week = max(count_per_week),
    count_per_month = max(count_per_month),
    count_per_30days = max(count_per_30days)
  ) %>%
    ungroup() %>%
  group_by(week_end) %>%
  mutate(
    location_per_week = toString(na.omit(location)),
    studyId_outbreakdetec_Patient_per_week = toString(na.omit(studyId_outbreakdetec_Patient)),
    AfnameDatum_LAB_per_week = toString(ifelse(!is.na(location), as.character(AfnameDatum_LAB_prior), "ABSENT")),
    Alarm_per_week2 = toString(ifelse(!is.na(location), Alarm_per_week, "ABSENT")),
    count_per_week2 = toString(ifelse(count_per_week != 0, count_per_week, NA)),
    count_per_month2 = toString(ifelse(count_per_month != 0, count_per_month, NA)),
    count_per_30days2 = toString(ifelse(count_per_30days != 0, count_per_30days, NA))
  ) %>%
  mutate(
    count_per_week2 = gsub(", NA", "", count_per_week2),
    count_per_month2 = gsub(", NA", "", count_per_month2),
    count_per_30days2 = gsub(", NA", "", count_per_30days2),
    count_per_week2 = gsub("NA, ", "", count_per_week2),
    count_per_month2 = gsub("NA, ", "", count_per_month2),
    count_per_30days2 = gsub("NA, ", "", count_per_30days2),
    count_per_week2 = gsub("NA", "", count_per_week2),
    count_per_month2 = gsub("NA", "", count_per_month2),
    count_per_30days2 = gsub("NA", "", count_per_30days2)
  )

# clean environment
rm( date_sequence, all_dates, goldPARR, goldPARR_merged )
```

``` r
# Below the cluster number and cluster ranges are added. This is done per cluster
# Note that the count that caused the alarm is different from the number of patients belonging to a cluster.
# A cluster is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store cluster numbers
cluster_numbers <- rep(NA, nrow(goldPARR_alarms))
cluster_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[i]
  current_location <- goldPARR_alarms$location[i]
  
  # Check if cluster number is already assigned within 30 days of the current row's date and same location
  if (is.na(cluster_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB_prior >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 & 
                              goldPARR_alarms$location == current_location)
    
    # Assign cluster number
    cluster_numbers[within_30_days] <- cluster_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB_prior >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 &
                                  goldPARR_alarms$location == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same cluster number to the next alarm
      cluster_numbers[next_alarm_index] <- cluster_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[next_alarm_index[1]]
    }
    
    # Increment cluster number for the next cluster
    cluster_num <- cluster_num + 1
  }
}

# Add cluster number to the data frame
goldPARR_alarms$cluster_number <- as.character(cluster_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(cluster_number2 = toString(ifelse(!is.na(location), cluster_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each cluster
cluster_ranges <- goldPARR_alarms %>%
  group_by(cluster_number, location) %>%
  summarize(start_date_cluster = min(week_end), end_date_cluster = max(week_end))
cluster_ranges <- subset(cluster_ranges, !is.na(cluster_ranges$cluster_number))
# Merge cluster_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, cluster_ranges, by = c("cluster_number"))
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(start_date_cluster), "ABSENT"))) %>%
  mutate(end_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(end_date_cluster), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL


# clean environment
rm( cluster_ranges, cluster_num, cluster_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below the alarm number and alarm ranges are added. This is done per location
# Note that the count that caused the alarm is different from the number of patients belonging to a alarm.
# A alarm is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store alarm numbers
alarm_numbers <- rep(NA, nrow(goldPARR_alarms))
alarm_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[i]
  current_location <- goldPARR_alarms$location.x[i]
  
  # Check if alarm number is already assigned within 30 days of the current row's date and same location
  if (is.na(alarm_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB_prior >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB_prior <= current_date & 
                              goldPARR_alarms$location.x == current_location)
    
    # Assign alarm number
    alarm_numbers[within_30_days] <- alarm_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB_prior >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 &
                                  goldPARR_alarms$location.x == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same alarm number to the next alarm
      alarm_numbers[next_alarm_index] <- alarm_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[next_alarm_index[1]]
    }
    
    # Increment alarm number for the next alarm
    alarm_num <- alarm_num + 1
  }
}

# Add alarm number to the data frame
goldPARR_alarms$alarm_number <- as.character(alarm_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(alarm_number2 = toString(ifelse(!is.na(location.x), alarm_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each alarm
alarm_ranges <- goldPARR_alarms %>%
  group_by(alarm_number, location.x) %>%
  summarize(start_date_alarms = min(week_end), end_date_alarms = max(week_end))
alarm_ranges <- subset(alarm_ranges, !is.na(alarm_ranges$alarm_number))
# Merge alarm_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, alarm_ranges, by = c("alarm_number"), keep = FALSE)
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(start_date_alarms), "ABSENT"))) %>%
  mutate(end_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(end_date_alarms), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( alarm_ranges, alarm_num, alarm_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below only the information of the Tuesdays is kept (because that is the day that the system is runned).
# all rows are splitted again, because all information was now in one row.
# Alarms that were raised despite a cluster size < 2 will be turned off.

# Filter rows where day_of_week is Tuesday
goldPARR_small <- goldPARR_alarms %>%
  filter(day_of_week == "Tuesday")

# Convert 'week_end' to week numbers
goldPARR_small <- goldPARR_small %>%
  mutate(week_number = week(AfnameDatum_LAB_prior))

# Remove the "ABSENT" from the strings to keep only the information needed
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "ABSENT, |ABSENT$", "")

goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "ABSENT, |ABSENT$", "")

# Split the location_per_week column into separate rows
goldPARR_small_2 <- goldPARR_small %>%
  mutate(
    location_per_week = gsub("^\\s+|\\s+$", "", location_per_week),
    studyId_outbreakdetec_Patient_per_week = gsub("^\\s+|\\s+$", "", studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = gsub("^\\s+|\\s+$", "", AfnameDatum_LAB_per_week),
    Alarm_per_week2 = gsub("^\\s+|\\s+$", "", Alarm_per_week2),
    count_per_week2 = gsub("^\\s+|\\s+$", "", count_per_week2),
    count_per_month2 = gsub("^\\s+|\\s+$", "", count_per_month2),
    count_per_30days2 = gsub("^\\s+|\\s+$", "", count_per_30days2),
    cluster_number2 = gsub("^\\s+|\\s+$", "", cluster_number2),
    alarm_number2 = gsub("^\\s+|\\s+$", "", alarm_number2),
    start_date_cluster2 = gsub("^\\s+|\\s+$", "", start_date_cluster2),
    end_date_cluster2 = gsub("^\\s+|\\s+$", "", end_date_cluster2),
    start_date_alarms2 = gsub("^\\s+|\\s+$", "", start_date_alarms2),
    end_date_alarms2 = gsub("^\\s+|\\s+$", "", end_date_alarms2)
  ) %>%
  separate_rows(location_per_week, studyId_outbreakdetec_Patient_per_week, AfnameDatum_LAB_per_week, Alarm_per_week2, count_per_week2, count_per_month2, count_per_30days2, cluster_number2, alarm_number2, start_date_cluster2, end_date_cluster2, start_date_alarms2, end_date_alarms2, sep = ",") %>%
  mutate(
    location_per_week = str_trim(location_per_week),
    studyId_outbreakdetec_Patient_per_week = str_trim(studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = str_trim(AfnameDatum_LAB_per_week),
    Alarm_per_week2 = str_trim(Alarm_per_week2),
    count_per_week2 = as.integer(str_trim(count_per_week2)),
    count_per_month2 = as.integer(str_trim(count_per_month2)),
    count_per_30days2 = as.integer(str_trim(count_per_30days2)),
    cluster_number2 = str_trim(cluster_number2),
    alarm_number2 = str_trim(alarm_number2),
    start_date_cluster2 = str_trim(start_date_cluster2),
    end_date_cluster2 = str_trim(end_date_cluster2),
    start_date_alarms2 = str_trim(start_date_alarms2),
    end_date_alarms2 = str_trim(end_date_alarms2)
)

# Change NA to "NA"
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_cluster2 = na_if(start_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_cluster2 = na_if(end_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_alarms2 = na_if(start_date_alarms2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_alarms2 = na_if(end_date_alarms2, "NA"))

# Convert to a Date
goldPARR_small_2$start_date_cluster2 <- as.Date(goldPARR_small_2$start_date_cluster2)
goldPARR_small_2$end_date_cluster2 <- as.Date(goldPARR_small_2$end_date_cluster2)
goldPARR_small_2$start_date_alarms2 <- as.Date(goldPARR_small_2$start_date_alarms2)
goldPARR_small_2$end_date_alarms2 <- as.Date(goldPARR_small_2$end_date_alarms2)
goldPARR_small_2$AfnameDatum_LAB_per_week <- as.Date(goldPARR_small_2$AfnameDatum_LAB_per_week)

# An alarm should only be raised if cluster_size is 2 or bigger
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    cluster_number2 = as.character(cluster_number2) # Ensure cluster_number2 is character
  ) %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2)
) %>%
  ungroup() %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(cluster_number2 = if_else(cluster_number2 %in% cluster_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, cluster_number2)) %>%
  ungroup()

# do also for alarm number
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    alarm_number2 = as.character(alarm_number2) # Ensure alarm_number2 is character
  ) %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2)
) %>%
  ungroup() %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(alarm_number2 = if_else(alarm_number2 %in% alarm_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, alarm_number2)) %>%
  ungroup()

goldPARR_small_2 <- goldPARR_small_2 %>% mutate(cluster_number2 = na_if(cluster_number2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(alarm_number2 = na_if(alarm_number2, "NA"))

# Split rows belonging to two patients
goldPARR_small_2 <- goldPARR_small_2 %>%
  separate_rows(studyId_outbreakdetec_Patient_per_week, sep = "_") 
```

``` r
# Filter and process gold_select dataset
gold_select_filtered <- gold_select %>%
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 | as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365) %>%
  ungroup() %>%
  mutate(studyId_outbreakdetec_Patient = as.character(studyId_outbreakdetec_Patient),
         year = as.character(year) )  # Convert to character

# Rename column in gold_select_filtered to match goldPARR_small_2
gold_select_filtered <- gold_select_filtered %>%
  rename(studyId_outbreakdetec_Patient_per_week = studyId_outbreakdetec_Patient)

# Convert studyId_outbreakdetec_Patient to character in goldPARR_small_2
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(studyId_outbreakdetec_Patient_per_week = as.character(studyId_outbreakdetec_Patient_per_week),
         year = as.character(year) )

# Perform the left join using the temporary match column
goldPARR_small_2 <- goldPARR_small_2 %>%
  left_join(gold_select_filtered %>%
              select(studyId_outbreakdetec_Patient_per_week, year, location_original = location, AfnameDatum_LAB_original = AfnameDatum_LAB),
            by = c("studyId_outbreakdetec_Patient_per_week", "year"))

# Remove the alarms that occur before the original AfnameDatum
goldPARR_small_2 <- goldPARR_small_2 %>%
  group_by(studyId_outbreakdetec_Patient_per_week) %>%
  mutate(Alarm_per_week2 = replace(Alarm_per_week2, duplicated(Alarm_per_week2, fromLast = TRUE), FALSE)) %>%
  ungroup()

# keep only the locations of interest
goldPARR_small_2 <- goldPARR_small_2 %>%
  filter(location_per_week == specific_location)

# Save for later
goldPARR_alarms_historyperWARD <- goldPARR_small_2
```

``` r
# The script below creates an heatmap to show the results for 2019

# Clean and process data
goldPARR_small_2_clean <- goldPARR_small_2 %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB_prior, "%m"))),
         year = format(AfnameDatum_LAB_prior, "%Y"),
         year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  group_by(year_month, location_per_week, Alarm_per_week2) %>%
  summarize(count_per_30days2 = sum(count_per_30days2), .groups = 'drop')

# Create a dataset with gaps for each year
years <- unique(format(goldPARR_small_2_clean$year_month, "%Y"))
gaps <- data.frame(
  year_month = as.Date(paste0(years, "-12-31")),
  location_per_week = rep(unique(goldPARR_small_2_clean$location_per_week), each = length(years)),
  Alarm_per_week2 = NA,
  count_per_30days2 = NA
)

# Combine the original data with the gaps
goldPARR_small_2_with_gaps <- bind_rows(goldPARR_small_2_clean, gaps) %>%
  arrange(year_month, location_per_week)

# Plot using ggplot2
goldPARR_plot_heatmap <- ggplot(goldPARR_small_2_with_gaps, aes(x = year_month, y = location_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 30, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Date", y = "Location", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = c(0, 0))

print(goldPARR_plot_heatmap)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/Plot%20goldPA%20movement%20data%201-1.png)<!-- -->

``` r
# clean environment
rm( aligned_plots, goldPARR_plot_heatmap, goldPA, goldPA.P75 )
```

**Fig. 6** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(with individual patient history - one point per location for ICU
cluster). Red boxes indicate the month where the P75 threshold was
crossed, green boxes are values below or equal to the threshold. A grey
box means that no threshold was available, possibly because that ward
was closed in the previous year, or no incidences occurred in the
previous year. Numbers in the boxes indicate the number of positive
tests.

  

#### P75 system - Thresholds including patient history (one point every day)

  

Now, every location the patient has visited in the 14 days preceding the
positive test will be counted. This counting haooend every day, meaning
that a longer stay will result in a higher count for a location.

  

``` r
# Remove duplicated rows
# Keep samples that are more than 1 year apart
goldPA <- gold_select %>% 
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 |
    as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365
  )
```

``` r
# The script below prepares the movement data and laboratory data for merging them.

# Rename columns in Patient_movements4_merged
Patient_movements1.rename <- Patient_movements4_merged %>%
  rename(
    #age_at_lab = age_index,
    Startdatum_OPNMUT = hos_mut_start_dt,
    Einddatum_OPNMUT = hos_mut_stop_dt,
    location = hos_mut_Afd_code,
    AfdelingOmschrijving_OPNMUT = hos_mut_afd_Omschrijving,
    Opnamedatum_OPN = hos_start_dt,
    Ontslagdatum_OPN = hos_stop_dt
  )

# Columns to be removed
columns_to_remove <- c(
  "umc_bezwaar_dt", "research2", "edu2", "recontact2", "data2", "bezwaar3", "bezwaar4", "recontract3",
  "AMIKACINE", "AMOXICILLINE", "AMPHOTERICINE_B", "AMPICILLIN", "AMPICILLINE", "ANIDULAFUNGIN",
  "AUGMENTIN", "AZITHROMYCINE", "AZTREONAM", "CASPOFUNGINE", "CEFAZOLINE", "CEFEPIME", "CEFOTAXIME",
  "CEFTAROLINE", "CEFTAZIDIME", "CEFTAZIDIME-AVIBACTAM", "CEFTOLOZANE_TAZOBACTAM", "CEFTRIAXON",
  "CEFUROXIM", "CEPHALOTHIN", "CHLOORAMPHENICOL", "CIPROFLOXACIN", "CLARITHROMYCINE", "CLINDAMYCINE",
  "CLOFAZIMINE", "CO_TRIMOXAZOL", "COLISTINE_POLYMYXINE", "CYCLOSERINE", "DAPTOMYCINE", "DOXYCYCLINE",
  "ERTAPENEM", "ERYTROMYCINE", "ESBL", "ETHAMBUTOL", "FLUCLOXACILLINE", "FLUCONAZOLE", "FLUCYTOSINE",
  "FOSFOMYCINE", "FUSIDINEZUUR", "GENTAMICINE", "HIGH_LEVEL_GENTAMICINE", "IMIPENEM", "INH.x",
  "ISAVUCONAZOL", "ITRACONAZOLE", "KETOCONAZOLE", "LEVOFLOXACIN", "LINEZOLID", "MEROPENEM", "METRONIDAZOLE",
  "MICAFUNGIN", "MICONAZOLE", "MINOCYCLINE", "MOXIFLOXACIN", "MUPIROCINE", "MUPIROCINE_H", "NEOMYCINE",
  "NITROFURANTOINE", "NORFLOXACIN", "OFLOXACIN", "OROTOMIDE", "PENICILLIN", "PENICILLINE", "PIPERA_TAZOBACTAM",
  "PIPERACILLINE", "POSACONAZOLE", "PROTIONAMIDE", "PYRAZINAMIDE", "QUINUPRISTINE", "RIFABUTINE", "RIFAMPICINE",
  "SPECTINOMYCINE", "STREPTOMYCINE", "TEICOPLANINE", "TEMOCILLINE", "TETRACYCLINE", "TIGECYCLINE", "TOBRAMYCINE",
  "TRIMETHOPRIM", "UNKOWN", "VANCOMYCINE", "VORICONAZOLE"
)

# Remove specified columns
goldPA.short <- goldPA[, !(names(goldPA) %in% columns_to_remove)]

# Ensure both dataframes have the same columns in the same order
common_columns <- union(names(goldPA.short), names(Patient_movements1.rename))
# Create an empty dataframe with all common columns and fill with NA
empty_df <- as_tibble(setNames(rep(list(NA), length(common_columns)), common_columns))

# Add missing columns with NA in goldPA.short
goldPA.new <- bind_cols(goldPA.short, empty_df %>% select(setdiff(common_columns, names(goldPA.short))))

# Add missing columns with NA in Patient_movements1.rename
Patient_movements1.new <- bind_cols(Patient_movements1.rename, empty_df %>% select(setdiff(common_columns, names(Patient_movements1.rename))))

# Order columns in the same order
goldPA.new <- goldPA.new[, common_columns]
Patient_movements1.new <- Patient_movements1.new[, common_columns]

# Merge rows for common studyId_outbreakdetec_Patient
merged_df_goldPA <- bind_rows(goldPA.new, Patient_movements1.new %>%
                         filter(studyId_outbreakdetec_Patient %in% goldPA.new$studyId_outbreakdetec_Patient))

# Format to a date
merged_df_goldPA$Opnamedatum_OPN <- as.Date(merged_df_goldPA$Opnamedatum_OPN)
merged_df_goldPA$Ontslagdatum_OPN <- as.Date(merged_df_goldPA$Ontslagdatum_OPN)

# Sort the data
merged_df_goldPA <- merged_df_goldPA %>% arrange( studyId_outbreakdetec_Patient )
# remove the same rows (that were present in both datasets before merging)
merged_df_goldPA <- merged_df_goldPA %>%
  distinct(studyId_outbreakdetec_Patient, Startdatum_OPNMUT, Einddatum_OPNMUT, .keep_all = TRUE)

# Clean environment
rm( columns_to_remove, goldPA.short, common_columns, empty_df  )
```

``` r
# The script below keeps the rows with the unique locations withing two weeks prior to the Afnamedatum, in this way they also get a count.

# Sort the data
goldPA.new <- goldPA.new %>%
  arrange(studyId_outbreakdetec_Patient, AfnameDatum_LAB)

# Create a new dataframe with 14 rows for each date
two.weeks_complete <- goldPA.new %>%
  group_by(studyId_outbreakdetec_Patient, AfnameDatum_LAB) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  complete(studyId_outbreakdetec_Patient, row_number = 1:14) %>%
  group_by(studyId_outbreakdetec_Patient) %>%
  fill(AfnameDatum_LAB, location) %>%
  mutate(AfnameDatum_LAB_prior = AfnameDatum_LAB - (row_number - 1))
# Remove unnecessary columns
two.weeks_complete <- select(two.weeks_complete, -row_number)
two.weeks_complete <- two.weeks_complete[,c("studyId_outbreakdetec_Patient","AfnameDatum_LAB_prior")] # "location",

# Merge the dataframes based on conditions
final_merged_df <- two.weeks_complete %>%
  full_join(merged_df_goldPA, by = "studyId_outbreakdetec_Patient") 

# Remove the rows with an Afnamedatum outside the Startdatum and Einddatum of the Opname
final_merged_df <- final_merged_df %>%
  filter(
    is.na(Einddatum_OPNMUT) |
    (AfnameDatum_LAB_prior >= Startdatum_OPNMUT & AfnameDatum_LAB_prior <= Einddatum_OPNMUT)
  ) 

# order the data
final_merged_df <- final_merged_df %>%
  arrange(studyId_outbreakdetec_Patient, desc(AfnameDatum_LAB_prior), by_group = TRUE)

# Remove duplicated rows and keep the first occurrence
final_merged_df <- distinct(final_merged_df, .keep_all = TRUE)

# Remove duplicated rows based on specified columns and keep the first occurrence
final_merged_df <- final_merged_df %>%
  distinct(studyId_outbreakdetec_Patient, AfnameDatum_LAB_prior, location, .keep_all = TRUE)

goldPA <- final_merged_df

# Clean environment
rm( goldPA.new, two.weeks_complete, final_merged_df  )
```

``` r
# Below we add the count per month, week and 30 days to the dataset 
# Also rows of multiple patients (in the same location) that has the same AfnameDatum are merged together

# Convert AfnameDatum_LAB_prior to a proper date format
goldPA$AfnameDatum_LAB_prior <- as.Date(goldPA$AfnameDatum_LAB_prior)
goldPA <- goldPA[order(goldPA$studyId_outbreakdetec_Patient),]
goldPA <- goldPA[order(goldPA$AfnameDatum_LAB_prior),]

# Step 1: Filter rows where AfnameDatum_LAB is not NA
filtered_df <- goldPA %>% 
  filter(!is.na(AfnameDatum_LAB)) %>% 
  select(studyId_outbreakdetec_Patient, Opnamedatum_OPN, Ontslagdatum_OPN, AfnameDatum_LAB)
filtered_df <- filtered_df[order(filtered_df$AfnameDatum_LAB),] 
filtered_df <- distinct(filtered_df)

# Step 3: Left join the original dataframe with the lookup table to fill in missing AfnameDatum_LAB values
filled_df <- goldPA %>%
  left_join(filtered_df, by = c("studyId_outbreakdetec_Patient", "Opnamedatum_OPN", "Ontslagdatum_OPN")) 
filled_df$AfnameDatum_LAB.x[is.na(filled_df$AfnameDatum_LAB.x)] <- filled_df$AfnameDatum_LAB.y[is.na(filled_df$AfnameDatum_LAB.x)]

# Remove extra column
goldPAR <- filled_df %>%
  select(-AfnameDatum_LAB.y) %>%
  rename(AfnameDatum_LAB = AfnameDatum_LAB.x)

# Group by location and date, then calculate the cumulative count
goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB_prior) %>%
  group_by(location, month = format(AfnameDatum_LAB_prior, "%Y-%m")) %>%
  mutate(count_per_month = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient,AfnameDatum_LAB_prior, AfnameDatum_LAB, location, count_per_month)

goldPAR <- goldPAR %>%
  arrange(AfnameDatum_LAB_prior) %>%
  mutate(week = format(AfnameDatum_LAB_prior - lubridate::wday(AfnameDatum_LAB_prior, week_start = 3) + 1, "%Y-%U")) %>% # per month: month = format(AfnameDatum_LAB_prior, "%Y-%m")
  group_by(location, week) %>% 
  mutate(count_per_week = row_number()) %>%
  ungroup() %>%
  select(studyId_outbreakdetec_Patient, AfnameDatum_LAB_prior, AfnameDatum_LAB, location, count_per_month, count_per_week)

# First calculate the dates from the first next Tuesday and the previous 30 days
goldPAR <- goldPAR %>%
  mutate(next_tuesday = AfnameDatum_LAB_prior + lubridate::days((9 - (lubridate::wday(AfnameDatum_LAB_prior) - 1) %% 7) %% 7),
         thirty_days_ago = next_tuesday - lubridate::days(30))

goldPAR2 <- goldPAR %>%
  left_join(goldPAR, by = "location") %>%
  group_by(location, studyId_outbreakdetec_Patient.x, AfnameDatum_LAB_prior.x, AfnameDatum_LAB.x) %>%
  summarize(count_per_30days = sum(AfnameDatum_LAB_prior.y >= thirty_days_ago.x & AfnameDatum_LAB_prior.y <= next_tuesday.x),
            .groups = "drop")

# Change colnames
colnames(goldPAR2) <- c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior", "AfnameDatum_LAB","count_per_30days")

# Add the counts from the 30 previous days
goldPARR <- merge(goldPAR, goldPAR2[, c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior", "count_per_30days")],
                   by = c("location", "studyId_outbreakdetec_Patient", "AfnameDatum_LAB_prior"),
                   all.x = TRUE) %>% 
  arrange(AfnameDatum_LAB_prior, studyId_outbreakdetec_Patient)
 
# Merge rows with the same AfnameDatum_LAB_prior
# Patients IDs are combined, separated by a _, the max count is used for that row
goldPARR <- goldPARR %>%
  group_by(AfnameDatum_LAB_prior, location) %>%
  summarize(AfnameDatum_LAB = AfnameDatum_LAB,
            #studyId_outbreakdetec_Patient = toString(unique(studyId_outbreakdetec_Patient)),
             studyId_outbreakdetec_Patient = paste(unique(studyId_outbreakdetec_Patient), collapse = '_'),         
            count_per_month = max(count_per_month),
             count_per_week = max(count_per_week),
             count_per_30days = max(count_per_30days))

# Add columns for month and year
goldPARR$month <- format(goldPARR$AfnameDatum_LAB_prior, "%m")
goldPARR$year <- format(goldPARR$AfnameDatum_LAB_prior, "%Y")

# clean environment
rm( filtered_df, filled_df, goldPA, goldPAR, goldPAR2 )
```

The P75 threshold is calculated based on the counts (calculated per 30
days) per location of the previous year.

``` r
# This script calculates the thresholds per year per location

# Add the weeknumber to the dataset
goldPARR <- goldPARR %>%
  mutate(week_number = week(AfnameDatum_LAB))

# Calculate the max. count per week/year/cluster
goldPARR2 <- goldPARR %>%
  group_by(week_number, year, month, location) %>%
  summarize(count_per_30days = max(count_per_30days))

# Calculate the threshold per year and cluster
goldPA.P75 <- goldPARR2 %>%
  group_by( year, location ) %>%
  summarize( Quantile_75 = quantile( count_per_30days, probs = 0.75 )) 

# Create a new column for the previous year's p75 value
goldPA.P75 <- goldPA.P75 %>%
  arrange(location, year) %>%
  group_by(location) %>%
  mutate(PrevYear_P75 = dplyr::lag(Quantile_75))

# Sort the data
goldPA.P75 <- goldPA.P75 %>% arrange( year, location )

# clean environment
rm( goldPARR2 )
```

``` r
# The script below adds the alarms to the data
# Because we simulate that the system is used every Tuesday all information is moved to that Tuesday.

# Generate a sequence of dates from January 2014 to December 2021
date_sequence <- seq( as.Date( "2014-01-01" ), as.Date( "2021-12-31" ), by = "day" )
# Create a dataframe with all possible dates
all_dates <- data.frame( AfnameDatum_LAB_prior = date_sequence )
# Merge the filtered dataframe with all_dates, filling missing values with 0
goldPARR_merged <- merge( all_dates, goldPARR, by = "AfnameDatum_LAB_prior", all.x = TRUE )
goldPARR_merged$count_per_30days[ is.na( goldPARR_merged$count_per_30days )] <- 0
goldPARR_merged$count_per_week[ is.na( goldPARR_merged$count_per_week )] <- 0
goldPARR_merged$count_per_month[ is.na( goldPARR_merged$count_per_month )] <- 0

# Fill in the new rows for month and year
goldPARR_merged$month <- format(goldPARR_merged$AfnameDatum_LAB_prior, "%m")
goldPARR_merged$year <- format(goldPARR_merged$AfnameDatum_LAB_prior, "%Y")
# Merge with goldPA.P75 to add PrevYear_P75 for each year
goldPARR_merged <- goldPARR_merged %>% left_join(goldPA.P75, by = c("year", "location"))
# Create a new column 'Above_Quantile' to represent whether count is above PrevYear_P75
goldPARR_merged$Above_Quantile <- goldPARR_merged$count_per_30days > goldPARR_merged$PrevYear_P75
# Remove the column with the threshold of that year (we use the one of the previous year)
goldPARR_merged <- select(goldPARR_merged, -c("Quantile_75" ))

# Create a new dataframe and add weekdays
goldPARR_alarms <- goldPARR_merged
goldPARR_alarms$day_of_week <- weekdays(goldPARR_alarms$AfnameDatum_LAB_prior)
goldPARR_alarms$day_of_week <- factor(goldPARR_alarms$day_of_week, levels = c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday"))

# Group by week and check if there was any Above_Quantile == TRUE in the previous week (including that day)
# Add all the variables from a certain week on the Tuesday
goldPARR_alarms <- goldPARR_alarms %>%
  arrange(AfnameDatum_LAB_prior) %>%
  mutate(week_end = as.Date(AfnameDatum_LAB_prior) + 6 - (as.numeric(format(AfnameDatum_LAB_prior, "%u")) - 3) %% 7) %>%
  group_by(week_end, location) %>%
  mutate(
    Alarm_per_week = ifelse(any(Above_Quantile == TRUE) & !all(Above_Quantile == FALSE | is.na(Above_Quantile)), TRUE, 
                            ifelse(all(is.na(Above_Quantile)), NA, FALSE)),
    count_per_week = max(count_per_week),
    count_per_month = max(count_per_month),
    count_per_30days = max(count_per_30days)
  ) %>%
    ungroup() %>%
  group_by(week_end) %>%
  mutate(
    location_per_week = toString(na.omit(location)),
    studyId_outbreakdetec_Patient_per_week = toString(na.omit(studyId_outbreakdetec_Patient)),
    AfnameDatum_LAB_per_week = toString(ifelse(!is.na(location), as.character(AfnameDatum_LAB_prior), "ABSENT")),
    Alarm_per_week2 = toString(ifelse(!is.na(location), Alarm_per_week, "ABSENT")),
    count_per_week2 = toString(ifelse(count_per_week != 0, count_per_week, NA)),
    count_per_month2 = toString(ifelse(count_per_month != 0, count_per_month, NA)),
    count_per_30days2 = toString(ifelse(count_per_30days != 0, count_per_30days, NA))
  ) %>%
  mutate(
    count_per_week2 = gsub(", NA", "", count_per_week2),
    count_per_month2 = gsub(", NA", "", count_per_month2),
    count_per_30days2 = gsub(", NA", "", count_per_30days2),
    count_per_week2 = gsub("NA, ", "", count_per_week2),
    count_per_month2 = gsub("NA, ", "", count_per_month2),
    count_per_30days2 = gsub("NA, ", "", count_per_30days2),
    count_per_week2 = gsub("NA", "", count_per_week2),
    count_per_month2 = gsub("NA", "", count_per_month2),
    count_per_30days2 = gsub("NA", "", count_per_30days2)
  )

# clean environment
rm( date_sequence, all_dates, goldPARR, goldPARR_merged )
```

``` r
# Below the cluster number and cluster ranges are added. This is done per cluster
# Note that the count that caused the alarm is different from the number of patients belonging to a cluster.
# A cluster is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store cluster numbers
cluster_numbers <- rep(NA, nrow(goldPARR_alarms))
cluster_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[i]
  current_location <- goldPARR_alarms$location[i]
  
  # Check if cluster number is already assigned within 30 days of the current row's date and same location
  if (is.na(cluster_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB_prior >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 & 
                              goldPARR_alarms$location == current_location)
    
    # Assign cluster number
    cluster_numbers[within_30_days] <- cluster_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which( goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB_prior >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 &
                                  goldPARR_alarms$location == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same cluster number to the next alarm
      cluster_numbers[next_alarm_index] <- cluster_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[next_alarm_index[1]]
    }
    
    # Increment cluster number for the next cluster
    cluster_num <- cluster_num + 1
  }
}

# Add cluster number to the data frame
goldPARR_alarms$cluster_number <- as.character(cluster_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(cluster_number2 = toString(ifelse(!is.na(location), cluster_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each cluster
cluster_ranges <- goldPARR_alarms %>%
  group_by(cluster_number, location) %>%
  summarize(start_date_cluster = min(week_end), end_date_cluster = max(week_end))
cluster_ranges <- subset(cluster_ranges, !is.na(cluster_ranges$cluster_number))
# Merge cluster_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, cluster_ranges, by = c("cluster_number"))
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(start_date_cluster), "ABSENT"))) %>%
  mutate(end_date_cluster2 = toString(ifelse(!is.na(location.x), as.character(end_date_cluster), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( cluster_ranges, cluster_num, cluster_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below the alarm number and alarm ranges are added. This is done per location
# Note that the count that caused the alarm is different from the number of patients belonging to a alarm.
# A alarm is not calculated on the Tuesday that the system is runned, but later on, because it also uses information of the future.

# Initialize a vector to store alarm numbers
alarm_numbers <- rep(NA, nrow(goldPARR_alarms))
alarm_num <- 1

# Iterate over each row where Above_Quantile == TRUE
for (i in which(goldPARR_alarms$Above_Quantile == TRUE)) {
  # Get the current row's date and location
  current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[i]
  current_location <- goldPARR_alarms$location.x[i]
  
  # Check if alarm number is already assigned within 30 days of the current row's date and same location
  if (is.na(alarm_numbers[i])) {
    # Find rows within a 30-day window centered around the current row's date and same location
    within_30_days <- which(goldPARR_alarms$AfnameDatum_LAB_prior >= current_date - 30 & 
                              goldPARR_alarms$AfnameDatum_LAB_prior <= current_date & 
                              goldPARR_alarms$location.x == current_location)
    
    # Assign alarm number
    alarm_numbers[within_30_days] <- alarm_num
    
    # Check for additional rows with Above_Quantile == TRUE of FALSE within the next 30 days
    while (TRUE) {
      next_alarm_index <- which(goldPARR_alarms$Above_Quantile == TRUE & !is.na(goldPARR_alarms$Above_Quantile) & #
                                  goldPARR_alarms$AfnameDatum_LAB_prior >= current_date + 1 &
                                  goldPARR_alarms$AfnameDatum_LAB_prior <= current_date + 30 &
                                  goldPARR_alarms$location.x == current_location)
      
      # If no more alarms found within 30 days, break the loop
      if (length(next_alarm_index) == 0)
        break
      
      # Assign the same alarm number to the next alarm
      alarm_numbers[next_alarm_index] <- alarm_num
      
      # Increment current date to the date of the next alarm
      current_date <- goldPARR_alarms$AfnameDatum_LAB_prior[next_alarm_index[1]]
    }
    
    # Increment alarm number for the next alarm
    alarm_num <- alarm_num + 1
  }
}

# Add alarm number to the data frame
goldPARR_alarms$alarm_number <- as.character(alarm_numbers)

# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end, location_per_week) %>%
  mutate(alarm_number2 = toString(ifelse(!is.na(location.x), alarm_number, "ABSENT"))) %>%
  ungroup()

# Calculate the start and end points for each alarm
alarm_ranges <- goldPARR_alarms %>%
  group_by(alarm_number, location.x) %>%
  summarize(start_date_alarms = min(week_end), end_date_alarms = max(week_end))
alarm_ranges <- subset(alarm_ranges, !is.na(alarm_ranges$alarm_number))
# Merge alarm_ranges
goldPARR_alarms <- left_join(goldPARR_alarms, alarm_ranges, by = c("alarm_number"), keep = FALSE)
# expand to whole week
goldPARR_alarms <- goldPARR_alarms %>%
  group_by(week_end) %>%
  mutate(start_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(start_date_alarms), "ABSENT"))) %>%
  mutate(end_date_alarms2 = toString(ifelse(!is.na(location.x.x), as.character(end_date_alarms), "ABSENT"))) %>%
  ungroup()

rownames(goldPARR_alarms) <- NULL

# clean environment
rm( alarm_ranges, alarm_num, alarm_numbers, current_date, current_location, i, next_alarm_index, within_30_days )
```

``` r
# Below only the information of the Tuesdays is kept (because that is the day that the system is runned).
# all rows are splitted again, because all information was now in one row.
# Alarms that were raised despite a cluster size < 2 will be turned off.

# Filter rows where day_of_week is Tuesday
goldPARR_small <- goldPARR_alarms %>%
  filter(day_of_week == "Tuesday")

# Convert 'week_end' to week numbers
goldPARR_small <- goldPARR_small %>%
  mutate(week_number = week(AfnameDatum_LAB_prior))

# Remove the "ABSENT" from the strings to keep only the information needed
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$Alarm_per_week2 <- str_replace_all(goldPARR_small$Alarm_per_week2, "ABSENT, |ABSENT$", "")

goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$cluster_number2 <- str_replace_all(goldPARR_small$cluster_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$alarm_number2 <- str_replace_all(goldPARR_small$alarm_number2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_cluster2 <- str_replace_all(goldPARR_small$start_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_cluster2 <- str_replace_all(goldPARR_small$end_date_cluster2, "ABSENT, |ABSENT$", "")

goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$start_date_alarms2 <- str_replace_all(goldPARR_small$start_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$end_date_alarms2 <- str_replace_all(goldPARR_small$end_date_alarms2, "ABSENT, |ABSENT$", "")

goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "(^\\s*ABSENT,\\s*|,\\s*ABSENT\\s*|\\s*ABSENT\\s*$)", "")
goldPARR_small$AfnameDatum_LAB_per_week <- str_replace_all(goldPARR_small$AfnameDatum_LAB_per_week, "ABSENT, |ABSENT$", "")

# Split the location_per_week column into separate rows
goldPARR_small_2 <- goldPARR_small %>%
  mutate(
    location_per_week = gsub("^\\s+|\\s+$", "", location_per_week),
    studyId_outbreakdetec_Patient_per_week =  gsub("^\\s+|\\s+$", "", studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = gsub("^\\s+|\\s+$", "", AfnameDatum_LAB_per_week),
    Alarm_per_week2 = gsub("^\\s+|\\s+$", "", Alarm_per_week2),
    count_per_week2 = gsub("^\\s+|\\s+$", "", count_per_week2),
    count_per_month2 = gsub("^\\s+|\\s+$", "", count_per_month2),
    count_per_30days2 = gsub("^\\s+|\\s+$", "", count_per_30days2),
    cluster_number2 = gsub("^\\s+|\\s+$", "", cluster_number2),
    alarm_number2 = gsub("^\\s+|\\s+$", "", alarm_number2),
    start_date_cluster2 = gsub("^\\s+|\\s+$", "", start_date_cluster2),
    end_date_cluster2 = gsub("^\\s+|\\s+$", "", end_date_cluster2),
    start_date_alarms2 = gsub("^\\s+|\\s+$", "", start_date_alarms2),
    end_date_alarms2 = gsub("^\\s+|\\s+$", "", end_date_alarms2)
  ) %>%
  separate_rows(location_per_week, studyId_outbreakdetec_Patient_per_week, AfnameDatum_LAB_per_week, Alarm_per_week2, count_per_week2, count_per_month2, count_per_30days2, cluster_number2, alarm_number2, start_date_cluster2, end_date_cluster2, start_date_alarms2, end_date_alarms2, sep = ",") %>%
  mutate(
    location_per_week = str_trim(location_per_week),
    studyId_outbreakdetec_Patient_per_week = str_trim(studyId_outbreakdetec_Patient_per_week),
    AfnameDatum_LAB_per_week = str_trim(AfnameDatum_LAB_per_week),
    Alarm_per_week2 = str_trim(Alarm_per_week2),
    count_per_week2 = as.integer(str_trim(count_per_week2)),
    count_per_month2 = as.integer(str_trim(count_per_month2)),
    count_per_30days2 = as.integer(str_trim(count_per_30days2)),
    cluster_number2 = str_trim(cluster_number2),
    alarm_number2 = str_trim(alarm_number2),
    start_date_cluster2 = str_trim(start_date_cluster2),
    end_date_cluster2 = str_trim(end_date_cluster2),
    start_date_alarms2 = str_trim(start_date_alarms2),
    end_date_alarms2 = str_trim(end_date_alarms2)
)

# Change NA to "NA"
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_cluster2 = na_if(start_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_cluster2 = na_if(end_date_cluster2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(start_date_alarms2 = na_if(start_date_alarms2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(end_date_alarms2 = na_if(end_date_alarms2, "NA"))

# Convert to a Date
goldPARR_small_2$start_date_cluster2 <- as.Date(goldPARR_small_2$start_date_cluster2)
goldPARR_small_2$end_date_cluster2 <- as.Date(goldPARR_small_2$end_date_cluster2)
goldPARR_small_2$start_date_alarms2 <- as.Date(goldPARR_small_2$start_date_alarms2)
goldPARR_small_2$end_date_alarms2 <- as.Date(goldPARR_small_2$end_date_alarms2)
goldPARR_small_2$AfnameDatum_LAB_per_week <- as.Date(goldPARR_small_2$AfnameDatum_LAB_per_week)

# An alarm should only be raised if cluster_size is 2 or bigger
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    cluster_number2 = as.character(cluster_number2) # Ensure cluster_number2 is character
  ) %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2)
) %>%
  ungroup() %>%
  group_by(cluster_number2, location_per_week) %>%
  mutate(cluster_number2 = if_else(cluster_number2 %in% cluster_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, cluster_number2)) %>%
  ungroup()

# do also for alarm number
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(
    Alarm_per_week2 = as.logical(Alarm_per_week2), # Ensure Alarm_per_week2 is logical
    alarm_number2 = as.character(alarm_number2) # Ensure alarm_number2 is character
  ) %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(
    Alarm_per_week2 = case_when(
      Alarm_per_week2 & n_distinct(studyId_outbreakdetec_Patient_per_week) < 2 ~ FALSE,
      TRUE ~ Alarm_per_week2)
) %>%
  ungroup() %>%
  group_by(alarm_number2, location_per_week) %>%
  mutate(alarm_number2 = if_else(alarm_number2 %in% alarm_number2[n_distinct(studyId_outbreakdetec_Patient_per_week) < 2], NA_character_, alarm_number2)) %>%
  ungroup()

goldPARR_small_2 <- goldPARR_small_2 %>% mutate(cluster_number2 = na_if(cluster_number2, "NA"))
goldPARR_small_2 <- goldPARR_small_2 %>% mutate(alarm_number2 = na_if(alarm_number2, "NA"))

# Split rows belonging to two patients
goldPARR_small_2 <- goldPARR_small_2 %>%
  separate_rows(studyId_outbreakdetec_Patient_per_week, sep = "_") 
```

``` r
# Filter and process gold_select dataset
gold_select_filtered <- gold_select %>%
  group_by(studyId_outbreakdetec_Patient) %>%
  filter(row_number() == 1 | as.Date(AfnameDatum_LAB) - lag(as.Date(AfnameDatum_LAB)) >= 365) %>%
  ungroup() %>%
  mutate(studyId_outbreakdetec_Patient = as.character(studyId_outbreakdetec_Patient),
         year = as.character(year))  # Convert to character

# Rename column in gold_select_filtered to match goldPARR_small_2
gold_select_filtered <- gold_select_filtered %>%
  rename(studyId_outbreakdetec_Patient_per_week = studyId_outbreakdetec_Patient)

# Convert studyId_outbreakdetec_Patient to character in goldPARR_small_2
goldPARR_small_2 <- goldPARR_small_2 %>%
  mutate(studyId_outbreakdetec_Patient_per_week = as.character(studyId_outbreakdetec_Patient_per_week),
         year = as.character(year))

# Perform the left join using the temporary match column
goldPARR_small_2 <- goldPARR_small_2 %>%
  left_join(gold_select_filtered %>%
              select(studyId_outbreakdetec_Patient_per_week, year, location_original = location, AfnameDatum_LAB_original = AfnameDatum_LAB),
            by = c("studyId_outbreakdetec_Patient_per_week", "year"))

# Remove the alarms that occur before the original AfnameDatum
goldPARR_small_2 <- goldPARR_small_2 %>%
  group_by(studyId_outbreakdetec_Patient_per_week) %>%
  mutate(Alarm_per_week2 = replace(Alarm_per_week2, duplicated(Alarm_per_week2, fromLast = TRUE), FALSE)) %>%
  ungroup()

# keep only the locations of interest
goldPARR_small_2 <- goldPARR_small_2 %>%
  filter(location_per_week == specific_location)

# Save for later
goldPARR_alarms_historyperDAY <- goldPARR_small_2
```

``` r
# The script below creates an heatmap to show the results for 2019

# Clean and process data
goldPARR_small_2_clean <- goldPARR_small_2 %>%
  mutate(month = sprintf("%02d", as.numeric(format(AfnameDatum_LAB_prior, "%m"))),
         year = format(AfnameDatum_LAB_prior, "%Y"),
         year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  mutate(location_per_week = ifelse(location_per_week == "", NA, location_per_week)) %>%  # Convert empty rows to NA
  filter(!is.na(location_per_week)) %>%  # Exclude rows where location_per_week is NA
  group_by(year_month, location_per_week, Alarm_per_week2) %>%
  summarize(count_per_30days2 = sum(count_per_30days2), .groups = 'drop')

# Create a dataset with gaps for each year
years <- unique(format(goldPARR_small_2_clean$year_month, "%Y"))
gaps <- data.frame(
  year_month = as.Date(paste0(years, "-12-31")),
  location_per_week = rep(unique(goldPARR_small_2_clean$location_per_week), each = length(years)),
  Alarm_per_week2 = NA,
  count_per_30days2 = NA
)

# Combine the original data with the gaps
goldPARR_small_2_with_gaps <- bind_rows(goldPARR_small_2_clean, gaps) %>%
  arrange(year_month, location_per_week)

# Plot using ggplot2
goldPARR_plot_heatmap <- ggplot(goldPARR_small_2_with_gaps, aes(x = year_month, y = location_per_week, fill = Alarm_per_week2)) +
  geom_tile(aes(width = 30, height = 0.9), color = "white") +
  geom_text(aes(label = count_per_30days2), vjust = 0.5) +  # Add this line for count labels
  scale_fill_manual(values = c("FALSE" = "#0CB702", "TRUE" = "red"), na.value = "grey") +
  labs(x = "Date", y = "Location", fill = "Above Quantile") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),  # Enlarge x-axis title
    axis.title.y = element_text(size = 18),  # Enlarge y-axis title
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = c(0, 0))

print(goldPARR_plot_heatmap)
```

![](GITHUB_AODS-with-patient-movements_P75_12062024_files/figure-gfm/Plot%20goldPA%20movement%20data%202-1.png)<!-- -->

``` r
# clean environment
rm( aligned_plots, goldPARR_plot_heatmap, goldPA, goldPA.P75 )
```

**Fig. 7** MDR Pseudomonas aeruginosa outbreak detection by P75 method
(with individual patient history - one point every day for ICU cluster).
Red boxes indicate the month where the P75 threshold was crossed, green
boxes are values below or equal to the threshold. A grey box means that
no threshold was available, possibly because that ward was closed in the
previous year, or no incidences occurred in the previous year. Numbers
in the boxes indicate the number of positive tests.

  
