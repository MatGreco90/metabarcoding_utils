library(DECIPHER)
library(tidyverse)
library(magrittr)
extract_euk_seqs_v9<-function(x){
  
  dna <- readDNAStringSet(x)
  dnadf<-as.data.frame(dna)
  dnadf$seq_name<-rownames(dnadf)
  
  final_df<-dnadf %>% 
    select(2,1) %>%
    set_colnames(c('seq_id','seq')) %>%  
    mutate(euk_sig=substr(seq, 1, 4)) %>% 
    mutate(keepers=ifelse(euk_sig=='GTCG','keep','drop')) %>% 
    filter(keepers=='keep')
  
  seq = final_df$seq
  names(seq) = final_df$seq_id
  dna = DNAStringSet(seq)
  
  writeXStringSet(dna,'onlyeuks.fasta')
}
