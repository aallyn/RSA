##### Merging Anthill Fits with Existing Fits April 2018
###########
library(tidyverse)
## Objective: We need to find common file names in the RSA GitHub results folder and in the AnthillFits_Winter2018 desktop folder. For common files, we need to merge all unique dataset groups run and then write out that newly compiled file. 

## Directories
github.dir<- "./Results/Fits/"
desk.dir<- "~/Desktop/AnthillFits_Winter2018/"
out.dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/"

## Subfolders
subs<- c("Weak", "Mod", "Strong")

## datasets.group.vec
reg<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
spglm<- paste(rep(reg, each = 20), seq(from = 1, to = 20, by = 1), sep = ".")

boot.ind<- seq(from = 1, to = 1000, by = 1)
morans.ind<- seq(from = 1, to  = 50, by = 1)
morans.check<- paste(rep(boot.ind, each = 50), morans.ind, sep = ".")

files.check<- "modstats" # Either globalmodresults, moransI, or modstats

for(i in seq_along(subs)){
  
  # Get the files from each location
  github.dir.use<- paste(github.dir, subs[i], "/", sep = "")
  desk.dir.use<- paste(desk.dir, subs[i], "/", sep = "")
  
  # Update pattern: globalmodresults, moransI, modstats
  git.files<- list.files(github.dir.use, files.check)
  desk.files<- list.files(desk.dir.use, files.check)
  
  # Locate common files
  shared.files<- intersect(git.files, desk.files)
  
  for(j in seq_along(shared.files)){
    file.use<- shared.files[j]
    git.file<- read.csv(paste(github.dir.use, file.use, sep = ""))
    desk.file<- read.csv(paste(desk.dir.use, file.use, sep = ""))
    
    # Get output directory subfolder
    if(any(grep("emm.ew", file.use))){
      sub.dir<- "Weak/"
    } else if(any(grep("emm.em", file.use))){
      sub.dir<- "Mod/"
    } else {
      sub.dir<- "Strong/"
    }
    
    # Check 
    if(files.check == "globalmodresults"){
      if(nrow(git.file) == 1000){
        write.csv(git.file, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        next
      } else {
        # Missing datasets.group
        if(any(grep("spglm", file.use))){
          git.groups<- unique(git.file$datasets.group)
          git.miss<- spglm[!(spglm %in% git.groups)]
          
          # Find those datasets.groups in desktop file
          desk.add<- desk.file[desk.file$datasets.group %in% git.miss,]
          
          # Add em to github file
          file.out<- bind_rows(git.file, desk.add)
          write.csv(file.out, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        } else {
          git.groups<- unique(git.file$datasets.group)
          git.miss<- reg[!(reg %in% git.groups)]
          
          # Find those datasets.groups in desktop file
          desk.add<- desk.file[desk.file$datasets.group %in% git.miss,]
          
          # Add em to github file
          file.out<- bind_rows(git.file, desk.add)
          write.csv(file.out, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        }
      }
    }
    
    if(files.check == "moransI"){
      git.file$boot.morans.check<- paste(git.file$boot.iter, git.file$dist.bin, sep = ".")
      git.file.red<- git.file[!duplicated(git.file$boot.morans.check),]
      git.miss<- morans.check[!(morans.check %in% unique(git.file.red$boot.morans.check))]
      
      # Find those datasets.groups in desktop file
      desk.file$boot.morans.check<- paste(desk.file$boot.iter, desk.file$dist.bin, sep = ".")
      desk.add<- desk.file[desk.file$boot.morans.check %in% git.miss,]
      
      if(nrow(desk.add) == 0){
        dat.temp<- data.frame("boot.morans.check" = as.character(git.miss), stringsAsFactors = F)
        dat.temp<- dat.temp %>%
          separate(boot.morans.check, c("boot.iter", "dist.bin"), sep = "[.]", remove = F) %>%
          mutate(., "moransI" = rep(NA, nrow(.))) %>%
          select(., boot.iter, dist.bin, moransI, boot.morans.check)
        dat.temp$boot.iter<- as.numeric(dat.temp$boot.iter)
        dat.temp$dist.bin<- as.numeric(dat.temp$dist.bin)
        file.out<- drop_na(bind_rows(git.file.red, dat.temp), boot.iter)
        write.csv(file.out, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        next
      } else {
        file.comb.temp<- drop_na(bind_rows(git.file.red, desk.add), boot.iter)
        if(nrow(file.comb.temp) == 50000){
          write.csv(file.comb.temp, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        } else {
          comb.miss<- morans.check[!(morans.check %in% unique(file.comb.temp$boot.morans.check))]
          dat.temp<- data.frame("boot.morans.check" = as.character(comb.miss), stringsAsFactors = F)
          dat.temp<- dat.temp %>%
            separate(boot.morans.check, c("boot.iter", "dist.bin"), sep = "[.]", remove = F) %>%
            mutate(., "moransI" = rep(NA, nrow(.))) %>%
            select(., boot.iter, dist.bin, moransI, boot.morans.check)
          dat.temp$boot.iter<- as.numeric(dat.temp$boot.iter)
          dat.temp$dist.bin<- as.numeric(dat.temp$dist.bin)
          file.out<- drop_na(bind_rows(file.comb.temp, dat.temp), boot.iter)
          write.csv(file.out, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
        }
      }
    }
    
    if(files.check == "modstats"){
      # Unique boot numbers
      git.check<- unique(git.file$boot)
      git.miss<- boot.iter[!(boot.iter %in% git.check)]
      
      if(length(git.miss) == 0){
        write.csv(git.file, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
      } else {
        # Find those boots in desktop file
        desk.add<- desk.file[desk.file$boot %in% git.miss,]
        
        # Add em to github file
        file.out<- bind_rows(git.file, desk.add)
        write.csv(file.out, file = paste(out.dir.use, sub.dir, file.use, sep = ""))
      }
    }
  }
}
