#!/usr/bin/env python3

# David Degnan, Pacific Northwest National Labs
## Last Updated: 2019_10_11
## Description: Converts sugar codes (Hex, etc.) and the Unimod chemical formula
##              strings to a collapsed format (i.e. C12H35O2 as opposed to 
##              C(12) H(35) O(2))

# Import necessary packages
import re

# First the chemical data was extracted from the glossary in R
# R Commands: Glossary <- read.csv("~/MS/mspecter/Glossary/UnimodGlossary.csv")
#             write.csv(Glossary$Composition, "~/MS/mspecter/Glossary/Comp.csv", 
#                       quote = F, row.names = F)

# Name the filetypes
file = "Comp.csv"
outFile = "MolForm.csv"

# Create a dictionary of sugar codes
sugar = {"Hep":"C7 H12 O6", "Hex":"C6 H10 O5", "HexA":"C6 H8 O6", 
         "HexN":"C6 H11 O4 N1", "HexNAc":"C8 H13 N1 O5", "Kdn":"C9 H14 O8",
         "NeuAc":"C11 H17 N1 O7", "NeuGc":"C11 H17 N1 O9", "Pent":"C5 H8 O5", 
         "Sulf":"S1 O3", "dHex":"C6 H10 O4", "Ac":"C2 H3 O1", 
         "13C":"C1 H1", "15N":"N1 H1", "18O":"O1 H2", "2H":"H2"}

# Create a function to convert sugar codes to molecular formulas
def convSug(sug, count):
    
    # Get the sugar code
    molForm = sugar[sug]
    
    # Split the list by element and number of instances
    num = list(map(int, re.sub("[aA-zZ()]", "", molForm).split()))
    
    # Multiply the number of instances by the count
    num = [x * count for x in num]
    
    # Add the element back in and return the molecular formula
    eleSyl = re.sub("[0-9()]", "", molForm).split()
    molForm = ""
    for pos in range(len(eleSyl)):
        molForm = molForm + eleSyl[pos] + str(num[pos]) + " "
    
    # Return the output
    return(molForm)

# Open the file and start a line counter
with open(file, "r") as fh:
    with open(outFile, "w") as oFH:
        oFH.write("MolecularFormula\n")
        lc = 0

        # For every line in the file, split it by the space
        for line in fh:
            lc += 1
            line = line.strip("\n").split()

            # Temporary dictionary with elements and count
            eleCount = {}

            # For every atom, add a one if no number quantity exsits
            for atom in line:
                atom = atom.strip(")").split("(")
                if (len(atom)) == 1:
                    atom.append("1")

                # If atom in sugar code dictionary, get molecular formula
                if atom[0] in sugar.keys():
                    atom[0] = convSug(atom[0], int(atom[1]))
                    atom[1] = ""

                # Fill the temp dictionary 
                eleCount[str(atom[0])] = str(atom[1])

            # String the molecular formula together
            molForm = ""
            for ele in eleCount:
                molForm = molForm + ele + eleCount[ele] + " "
            molForm = molForm + "\n"
            oFH.write(molForm)