#!/usr/bin/env python3

## David Degnan, Pacific Northwest National Labs
## Last Updated: 2019_08_27
## Description: Retrieves modification site (amino acid) information from unimod.odo,
##              writing output in its own file. 

file = "unimod.txt"
outFile = "./Unimod_Sites.csv"

UniSite = dict()

Unimod = ""

with open(file, "r") as fh:
    lc = 0
    for line in fh:
        lc += 1
        if lc % 500 == 0:
            print(lc)
        if line.startswith("id: UNIMOD"):
            Unimod = str(line.split(":")[-1].strip("\n"))
        if "site" in line and "xref" in line:
            site = line.split('"')[1]
            if (Unimod in UniSite.keys()) == False:
                UniSite[Unimod] = [site]
            else:
                UniSite[Unimod].append(site)
                
                
with open(outFile, "w") as oFH: 
    oFH.write("Accession.Number,Modified.Sites\n")
    lc = 0
    for Unimod in UniSite.keys():
        lc += 1
        if lc % 500 == 0:
            print(lc)
        outString = Unimod + "," + " ".join(set(UniSite[Unimod])) + "\n"
        oFH.write(outString)

