source("../global.R")


plot_spectral_bait_condition_using_uniprot<- function(uniprot_id){
  
  # filter data on substring using grepl, leaves the original uniprot annotation intact 
  my_data <- filter(all_data, grepl(uniprot_id, uniprot)) %>% 
    group_by(bait, condition) %>% 
    # .groups = drop is the default, will get warnings if you not specify explicitly 
    summarise(.groups = 'drop', across(spectral_count, sum)) %>% 
    unite(bait_condition, c(bait, condition), sep = "_")
  
  p <- ggplot(my_data, aes(x = bait_condition, y = spectral_count))
  p <- p + geom_col() + labs(title = uniprot_id)
  p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_spectral_bait_condition_using_uniprot('P36415')
plot_spectral_bait_condition_using_uniprot('GST')
plot_spectral_bait_condition_using_uniprot('Q54YT4')