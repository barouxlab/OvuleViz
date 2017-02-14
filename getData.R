
if(!'pacman' %in% rownames(installed.packages())) install.packages('pacman')
pacman::p_load(stringr, tcltk)

############################################################
# set directory where Imaris data is stored
############################################################

while(1) {
  dir <- tk_choose.dir(caption = "Go to the folder where Imaris files are")
  #print('The following files will be imported:')
  files <- dir(dir, recursive = TRUE)
  if(length(grep('.csv', files)) == length(files)){
    cat('Importing', length(files), 'files...\n\n')
    break} else {
      warning('Selected directory should contain only .csv files. Choose another directory.',
              call. = FALSE, immediate. = TRUE)
    }
}
setwd(dir)

############################################################
# retrieve csv files
############################################################

# retrieve all "*Detailed*" .csv files from each of the subfolders,
# skip the first 3 rows and pool in the "raw.data" dataframe

raw.data <- data.frame() 

for(genotype in list.files()){
  for(stage in list.files(path = genotype)){
    for(stack in list.files(path = paste(genotype,stage, sep = "/"))){
      temp <- NULL
      try(temp <-  read.table(file=list.files(path = paste(genotype,stage,stack, sep = "/"),
                                          pattern = "Detailed",
                                          full.names = TRUE),
                          skip = 3, sep = ",", header = TRUE))
      if(!is.null(temp)){ # skips adding rows if file did not exist
        temp$Genotype <- genotype
        temp$Stage <- stage
        temp$Stack <- stack
        tryCatch(raw.data <- rbind(raw.data, temp), error = function(x){
          stop(call. = FALSE, c("Cannot import ", stack, " (", genotype, "/", stage, ")"))
        })
      }
    }
  }
}

############################################################
# change order and name of the columns
############################################################

data <- with(raw.data, data.frame(Stack = Stack,
                              Genotype=Genotype,
                              Stage = Stage,
                              Type = Variable,
                              Value = Value,
                              Labels = str_split_fixed(as.character(Default.Labels), ";",2)[,1],
                              neighbourSMC = str_split_fixed(as.character(Default.Labels), ";",2)[,2]
                              ))

############################################################
# Check tags
############################################################

# confirm that only one label (+ optionalSMC tag) was used
if(length(levels(data$neighbourSMC)) > 2){
  warning('\nMultiple labels used! Only the first one will be used. Incorrect values are on stacks:',
          call. = FALSE, immediate. = TRUE)
  cat(paste0('\n', unique(raw.data[!data$neighbourSMC %in% c("", "SMC contact"), 'Stack'])))
}

# reformat neightbour SMC field
data$neighbourSMC <- factor(data$neighbourSMC, levels = c('SMC contact', 'no SMC contact'))
data$neighbourSMC[is.na(data$neighbourSMC)] <- 'no SMC contact'


############################################################
# Check Labels
############################################################

# confirm that all labels are in the allowed list
# accepted Labels:

if(!exists('TrueLabel')){
  TrueLabel <- c("", "CC", "L1 apical",
                 "L1 basal", "L1 basal sup", "L1 dome", "L2 apical",
                 "L2 basal", "L2 basal sup", "pSMC", "SMC")
}

cat('\n\n**************************************
    Only the following Labels will be accepted:\n',
    paste0('"', TrueLabel, '"\n'),
    '\nTo change this list, rerun this script with the following line at the start:\n',
    'TrueLabel <- c("", "CC", ...)\n\n')

if(!all(levels(data$Labels) %in% TrueLabel)){
  wrongLabel <- data[!data$Labels %in% TrueLabel,]
  warning(nrow(wrongLabel), ' values were not in the allowed list! They will not be used.',
          call. = FALSE, immediate. = TRUE)
  w <- factor(wrongLabel$Labels)
  cat('Unaccepted label(s): ', levels(w), '\n on stacks:',
      paste('\n', levels(factor(wrongLabel$Stack))))
  data <- data[data$Labels %in% TrueLabel,]
  data$Labels <- factor(data$Labels)
}

############################################################
# Check Type
############################################################

if(!exists('TrueType')){
  TrueType <- c("Cell Area", "Cell Ellipticity (oblate)",
                "Cell Ellipticity (prolate)", "Cell Sphericity", "Cell Volume")}

cat('\n\n**************************************
    Only the following Types will be accepted:\n',
    paste0('"', TrueType, '"\n'),
    'To change this list, rerun this script with the following line at the start:\n',
    'TrueType <- c("Cell Area", ...)\n\n')

if(!all(levels(data$Type) %in% TrueType)){
  
  wrongType <- data$Type[!data$Type %in% TrueType]
  warning(nrow(wrongType), 'The following Types were removed:',
          call. = FALSE, immediate. = TRUE)
  cat(levels(factor(wrongType)))
  data <- data[data$Type %in% TrueType,]
  data$Type <- factor(data$Type)
}

############################################################
# print summary
############################################################

cat('\n\n**************************************
    SUMMARY\n\nImported data:', dim(data)[1], 'observations',
    '\n\n-->', length(levels(data$Genotype)), 'genotypes:', levels(data$Genotype),
    '\n-->', length(levels(data$Stage)), 'stages:', levels(data$Stage),
    '\n-->', length(levels(data$Type)), 'types:', levels(data$Type),
    '\n-->', length(levels(data$Labels)), 'labels:', levels(data$Labels),
    '\n-->', table(data$neighbourSMC)[1], 'cells have SMC contact.')

##############################################################
# Export as csv file
##############################################################
setwd('..')
write.csv2(data, file = tclvalue(tkgetSaveFile(initialfile = "segmented.csv")),
           row.names = FALSE)
