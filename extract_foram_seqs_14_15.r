library(DECIPHER)
library(tidyverse)
library(magrittr)
extract_foram_seqs_14_15<-function(x){
  
  dna <- readDNAStringSet(x)
  dnadf<-as.data.frame(dna)
  dnadf$seq_name<-rownames(dnadf)
  
  final_df<-dnadf %>% 
    select(2,1) %>%
    set_colnames(c('seq_id','seq')) %>%  
  mutate(seq_len=nchar(seq)) %>% 
  filter(seq_len>70) %>% 
  filter(grepl('GACAG',seq, fixed = TRUE)) %>%    
  filter(grepl('GGAGCATGT|GGAGTATGT',seq))

  seq = final_df$seq
  names(seq) = final_df$seq_id
  dna = DNAStringSet(seq)
  
  writeXStringSet(dna,'onlyforams.fasta')
}
