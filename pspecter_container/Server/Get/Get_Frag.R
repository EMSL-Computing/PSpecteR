## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_05_11

# DESCROPTION: Gets the getters used by getFrag: getCalcFrag, addModMass, 
# applyMod, getMolecularFormula, and getCalcIso, getAnnotatedPeaks, and getCorrelationScore.

list(
  
# Function Description: Calculate all theoretical fragments from a sequence.
# Inputs: seq (new seq), ionGroups, charge
# Function: Use calculateFragments (MSnbase) with other code to clean output
# Outputs: Fragment data: mz, ion, type, pos, z, seq, npos
getCalcFrag <- function(seq, ionGroups, charge) {
  
  # If there is no sequence data, return NULL
  if (is.na(seq)) {return(NULL)}
  
  # Calculate fragment data with MSnbase 
  frag <- calculateFragments(sequence = seq, type = ionGroups, z = charge)
  
  # Exclude N-deamidated and C-dehydrated specific modifications
  frag <- filter(frag, !grepl("[.*_]", frag$type))
  
  # Create a N-pos for plotting features, as well as maintaining pos
  if (nrow(frag) != 0) {
    
    # Change position values to be the same direction
    frag$npos <- lapply(1:nrow(frag), function(i) {
      if (frag$type[i] %in% c("x", "y", "z")) {
        (nchar(seq) + 1) - frag$pos[i]} else {frag$pos[i]}}) %>% unlist()
  }
  
  return(frag)
},

# Function Description: Adjust fragment data to account for modifications
# Input: mod data (spectrumID, sequence, name, mass, location),
# Function: Calculate modification masses and formulas
# Output: Mass and formula modifications per fragment's position (not n or c)
addModMass <- function(mod, frag, scanNum, seq, Glossary) {
  
  # Get modifications for specific scanNum and seq
  modPerSeq <- mod[mod$spectrumID == scanNum & mod$sequence == seq,]
  
  # Determine mass to be added to an abc or xyz fragment based on the fragment's length.
  # For a sequence of length 5, the a modification of 8 Da at position 4 would result
  # in abcMass = (0,0,0,1,1) and xyzMass = (0,1,1,1,1). Do the same for the molecular
  # formula, i.e. abcForm = ("", "", "", "H", "H") and xyzForm = ("", "H", "H", 
  # "H", "H"). 
  len <- nchar(seq)
  abcMass <- xyzMass <- rep(0, len)
  abcForm <- xyzForm <- rep("", len)
  
  # Ensure that there are some modifications to apply
  if (is.null(modPerSeq) == F) {if (nrow(modPerSeq) != 0) {
  
    for (row in 1:nrow(modPerSeq)) {
      
      r <- modPerSeq[row,]
      form <- ""
      if (grepl("Added_Mass", r$name) == F) {
        form <- as.character(Glossary[r$name == Glossary$Interim.Name, 8][1])
      }
      
      # If C-position is 0, that means the C-terminal mass has no effect on ABC fragments.
      if (r$cpos != 0) {
        abcMass[r$location:r$length] <- abcMass[r$location:r$length] + r$mass
        abcForm[r$location:r$length] <- paste(abcForm[r$location:r$length], form, sep = "")
      }
      
      # Since xyz is the reverse order, take the Nth Position of the Reverse Order (npro)
      # If the location is 0, that means the N-terminal mass has no effect on XYZ fragments.
      if (r$location != 0) {
        npro <- (r$length:1)[r$location]
        xyzMass[npro:r$length] <- xyzMass[npro:r$length] + r$mass
        xyzForm[npro:r$length] <- paste(xyzForm[npro:r$length], form, sep = "")
      }
    }
  }}
  
  return(data.frame(abcMass, abcForm, xyzMass, xyzForm, stringsAsFactors = F))
},


# Function Description: Take the modification output and add it to frag
# Input: frag, modMass
# Function: Add modMass data (mass and formula) to frag
# Outputs: Fragment data: mz (adjusted), ion, type, pos, z, seq, npos, formula
applyMod <- function(modMass, frag) {

  data.frame(do.call(rbind, lapply(1:nrow(frag), function(row) {
    theoPeak <- frag[row,]
    
    if (theoPeak$type %in% c("a", "b", "c")) {
      theoPeak$mz <- theoPeak$mz + (modMass$abcMass[theoPeak$pos] / theoPeak$z)
      theoPeak$molForm <- modMass$abcForm[theoPeak$pos]
    }
    if (theoPeak$type %in% c("x", "y", "z")) {
      theoPeak$mz <- theoPeak$mz + (modMass$xyzMass[theoPeak$pos] / theoPeak$z)
      theoPeak$molForm <- modMass$xyzForm[theoPeak$pos]
    }
    
    return(theoPeak)
  })))
},

## THE FOLLOWING FUNCTIONS ARE HELPERS TO getMolecularFormula()

# Molecular structure is different depending on the fragment type
adjustForFragType <- function(type, seq) {
  at <- getAtomsFromSeq(seq)
  if (type == "a") {at$C <- at$C - 1; at$H <- at$H - 1; at$O <- at$O - 2} else
  if (type == "b") {at$H <- at$H - 1; at$O <- at$O - 1} else
  if (type == "c") {at$H <- at$H + 2; at$N <- at$N + 1; at$O <- at$O - 1} else
  if (type == "x") {at$C <- at$C + 1; at$H <- at$H - 1; at$O <- at$O + 1} else
  if (type == "y") {at$H <- at$H + 1} else
  if (type == "z") {at$H <- at$H - 2; at$N <- at$N - 1} 
  return(at)
},

# Add modifications atoms to the molecule list
stringToList <- function(modForm) {
  modSplit <- strsplit(modForm, " ")
  NUM <- lapply(modSplit, function(el) {as.numeric(gsub("[[:alpha:]]", "", el))}) %>% unlist()
  names(NUM) <- lapply(modSplit, function(el) {gsub("[[:digit:]]", "", el)}) %>% unlist()
  modForm <- as.list(NUM)
  return(modForm)
},

# Add modification atoms to fragment atoms, remove zeroes, and print output
addAtoms <- function(fragForm, modForm) {
  
  # If no modifications exist, just return the formula for fragmented data
  if (length(modForm) == 0) {
    
    # Remove zeroes, and then paste together
    fragForm <- fragForm[fragForm > 0]
    return(paste0(names(fragForm), unlist(fragForm), collapse = ""))}
  
  # Add modifications atoms to fragment atoms
  fragAtoms <- names(fragForm)
  for (pos in 1:length(modForm)) {
    el <- names(modForm[pos]); num <- modForm[pos]
    ifelse(el %in% fragAtoms, fragForm[[el]] <- fragForm[[el]] + modForm[[el]],
           fragForm[[el]] <- modForm[[el]])
  }
  
  # Remove Zeroes
  fragForm <- fragForm[fragForm > 0]
  
  # Collapse formula
  return(paste0(names(fragForm), unlist(fragForm), collapse = ""))
},

# Function Description: Determine the molecular formula based on the fragment and any modifications
# Input: fragment data from applyMod
# Function: Convert to sequences to a list of atoms, accounting for fragments and modifications
# Outputs: Fragment data: mz (adjusted), ion, type, pos, seq, npos, formula
getMolecularFormula <- function(frag) {
  
  # Adjust the literature molecular formula for fragmentation and modifications
  MolForm <- do.call(rbind, lapply(1:nrow(frag), function(row) {
    fragForm <- adjustForFragType(frag$type[row], frag$seq[row])
    modForm <- stringToList(frag$molForm[row])
    return(addAtoms(fragForm, modForm))
  }))
  
  frag$molForm <- as.character(unlist(MolForm))
  return(frag)
},

# Function Description: Calculate the isotopic distribution based off the sequence
# Input: fragment from getMolecularFormula
# Function: Uses the getMolecule call from Rdisop and return the isotopic distribution
# Outputs: Fragment data: mz (adjusted), ion, type, pos, seq, npos, formula, isotopic distribution
getCalcIso <- function(frag, isoper) {
  
  # Define the weight of a proton
  p <- 1.003
  
  # Get all the isotopes
  Isotopes <- data.frame(do.call(rbind, lapply(frag$molForm, function(molForm) {
    
    # Get the molecular formula, and calculate the isotope peak distribution, accounting
    # for the filter
    iso <- getMolecule(molForm)$isotopes[[1]][2,]
    iso <- iso[iso > isoper]
    
    # If no isotopes returned, just return NA
    if (length(iso) == 0) {return(cbind(molForm, NA, NA))}
    
    # Split the Isotopic peak information
    isotope <- strsplit(as.character(iso), " ") %>% unlist()
    isoPeak <- paste("M+", 0:(length(isotope) - 1), sep = "")
    
    # Return result 
    return(cbind(molForm, isotope, isoPeak))
    
  })))
  
  # If no isotopes, return NULL
  if (is.null(Isotopes) || is.na(unique(Isotopes[,2]))) {return(NULL)}
  
  # Merge Isotopes into frag
  frag <- unique(merge(frag, Isotopes, by = "molForm", sort = F))
  frag <- frag[,c("mz", "ion", "type", "pos", "z", "seq", "npos", "molForm", "isotope", "isoPeak")]
  
  # Format mz
  frag$mz <- frag$mz + ((as.numeric(gsub("M+", "", frag$isoPeak, fixed = T)) * p) / frag$z)
  frag$isotope <- as.numeric(as.character(frag$isotope))
  frag$isoPeak <- as.character(frag$isoPeak)
  rownames(frag) <- 1:nrow(frag)

  return(frag)
},

# Function Description: Returns annotated peaks
# Inputs: fragment from getCalcIso, ms (with ms and intensity labelled), mztolppm
# Function: Returns a data frame with all theoretical peaks in theor 
# that are found in the experimental spectrum ms
# Output: annotated frag with closestIndexExp, mzExp, intensityExp
getAnnotatedPeaks <- function(ms, theor, mztolppm) {
  
  # Validate arguments:
  if (mztolppm < 0) {return(NULL)}
  if (nrow(ms) < 1 || nrow(theor) < 1) {return(NULL)}
  
  theor$mztol <- theor$mz * (mztolppm / 1e6)
  
  # For each theor, find the closest index in ms, where ms = theor:
  leftIndex <- findInterval(theor$mz, ms$mz, rightmost.closed = FALSE, all.inside = TRUE)
  
  # Compute mz differences (absolute) to closest element to each side, smaller to the left 
  # and next greater to the right:
  theor$leftDiff <- abs(ms$mz[leftIndex] - theor$mz)
  theor$rightDiff <- abs(ms$mz[leftIndex + 1] - theor$mz)
  theor$closestIndexExp <- leftIndex
  
  # Set closest index as right side one, if difference is smaller:
  rightIndexBest <- which(theor$rightDiff < theor$leftDiff)
  theor$closestIndexExp[rightIndexBest] <- theor$closestIndexExp[rightIndexBest] + 1
  theor$mzDiff <- abs(ms$mz[theor$closestIndexExp] - theor$mz)
  theor <- theor[which(theor$mzDiff < theor$mztol), ] # keep only matches within tolerance
  theor$mzExp <- ms$mz[theor$closestIndexExp]
  theor$intensityExp <- ms$intensity[theor$closestIndexExp]
  theor$leftDiff <- theor$rightDiff <- theor$mztol <- theor$mzDiff <- NULL
  return(theor)
},

# Function Description: Calculates the correlation score for isotopes
# Inputs: fragment from getAnnotatedPeaks
# Function: Calculates correlation score data for isotopes
# Output: returns correlation score
getCorrelationScore <- function(frag) {
  
  # Make a fragment group category and count the number of instances of those groups
  frag$fragGroup <- as.factor(paste(frag$ion, "_", frag$z, sep = ""))
  
  # Create empty vector to hold correlation score values
  corrScore <- c()

  # Calculate correlation score for isotopes per frag group and turn into a dataframe
  for (group in unique(frag$fragGroup)) {
    fragGroup <- frag[frag$fragGroup == group,]
    if (nrow(fragGroup) < 3) {corrScore <- c(corrScore, NA)} else {
      corrScore <- c(corrScore, cosine(fragGroup$intensityExp, fragGroup$isotope))}
  }
  corrScore <- data.frame("fragGroup" = unique(frag$fragGroup), "corrScore" = round(corrScore, 6))
  
  # Merge correlation score to the fragment dataframe and return results
  frag <- merge(frag, corrScore, merge = c("fragGroup", "fragGroup"), all = T)
  return(frag[,c(2:16)])

}

)
