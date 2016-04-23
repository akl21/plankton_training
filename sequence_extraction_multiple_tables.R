extraction_table <- read.csv(file = "data/extraction_data.csv", 
                      stringsAsFactors = FALSE)
pcr_table <- read.csv(file = "data/pcr_data.csv", 
                      stringsAsFactors = FALSE)
phylum_listings <- read.csv(file = "data/phylum_listings.csv", 
                      stringsAsFactors = FALSE)
sequencing_plate_data <- read.csv(file = "data/sequencing_plate_data.csv",
                                  stringsAsFactors = FALSE)
template_file <- read.csv(file = "20160405-table-template.csv",
                          stringsAsFactors = FALSE)
library("dplyr")

#This code block outputs a table showing whether or not the sample has had a 
#sequence extraction: 1 if it did, 0 if not
has_seq_extract = function(phylum_listings, extraction_table){
  phylum_listings %>%
    left_join(extraction_table) %>%
    group_by(voucher_number) %>%
    summarize(has_extraction = n())
}
tt = has_seq_extract(phylum_listings, extraction_table)
tt



#Here I find the number of PCR sequencing attempts for each voucher.
count_pcr_trials <- function(pcr_data){
  select(pcr_data, voucher_number, pcr_success) %>%
    group_by(voucher_number) %>%
    summarize(number_pcr_attempts = n())
    
}
tt1 <- count_pcr_trials(pcr_table) %>%
  left_join(phylum_listings) %>%
  select(voucher_number, phylum, number_pcr_attempts)

#This code module generates a table showing which vouchers were successful at PCR.
summarize_pcr_successes <- function(pcr_data){
  select(pcr_data, voucher_number, pcr_success) %>%
    filter(pcr_success==1) %>%
    group_by(voucher_number) %>%
    summarise(pcr_successful= n())
  
}
tt2 <- summarize_pcr_successes(pcr_table)

#In this code module I try to create a table named has_seq_file using the list.files()
#function that shows whether each voucher as a sequence file associated with it. 
has_seq_file <- function(seq_path, pcr_table) {
  seq_files <-  list.files(path = seq_path) 
  has_seq_file <-  pcr_table[,"voucher_number"] %in% seq_files
  has_seq_file <- as.numeric(has_seq_file)
  has_seq_file <-  data.frame("voucher_number" = pcr_table[,"voucher_number"],
                              "has_sequence_file" = has_seq_file, stringsAsFactors = FALSE)
  has_seq_file
}
tt3 <- has_seq_file("seqs/COI", pcr_table)
tt3

#This code block produces a table that lists whether or not each 
#voucher was successful in being sequenced.
seq_success = function(sequencing_plate_data, pcr_table){
  tt4 <- sequencing_plate_data %>%
    left_join(pcr_table) %>%
    select(voucher_number, sequence_success = success)
  tt4
}
tt4 = seq_success(sequencing_plate_data, pcr_table)
tt4
sample_status = function(pcr_tbl = pcr_table, 
                         ext_tbl = extraction_table,
                         phy_listings = phylum_listings, 
                         seq_plate_data = sequencing_plate_data,
                         seq_pth = "seqs/COI"){
  tt <-  has_seq_extract(phy_listings, ext_tbl)
  tt1 <- count_pcr_trials(pcr_tbl) %>%
    left_join(phy_listings) %>%
    select(voucher_number, phylum, number_pcr_attempts)
  tt2 <- summarize_pcr_successes(pcr_tbl)
  tt3 <- has_seq_file(seq_pth, pcr_tbl)
  tt4 <-  seq_success(seq_plate_data, pcr_tbl)
  tt %>%
    left_join(tt1) %>%
    left_join(tt2) %>%
    left_join(tt3) %>%
    left_join(tt4)
}