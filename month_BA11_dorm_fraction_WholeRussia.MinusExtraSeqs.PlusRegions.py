import sys
import ete3
import itertools
import random
import re
from collections import defaultdict
import time

oldseqs = list()
with open("RussiaBA11InTree.tsv") as old: #list of Russian samples from the UShER tree of BA.1.1
    for line in old:
        name = line.split("|")[0]
        name1 = name.split("/")[1]
        oldseqs.append(name1)
        

month_all = defaultdict(int)
month_omicron = defaultdict(int)
month_ba11 = defaultdict(int)

#month_all['2021-12'] = -120
#month_omicron['2021-12'] = -120
#month_ba11['2021-12'] = -120

dorm_ids = list()
with open ("dorm_samples.tsv") as dorm: #list of dormitory samples
    for line in dorm:
        line1 = line.split("_")[2]
        name = line1.split("/")[0]
        name = str(name)
        dorm_ids.append(name)
month_dorm = defaultdict(int)
reg_b11 = defaultdict(int)
reg_b11_dorm = defaultdict(int) 

with open ("Russia.FromGISAIDMetdata.CompleteCovered.InTree.tsv") as metadata: #metadata of Russian sequences that are present in UShER tree
    for line in metadata:
        name = line.split("\t")[0]
        subname = name.split("/")[2]
        region = subname.split("-")[0]
        date = line.split("\t")[3]
        lineage = line.split("\t")[11]
        subm_date = line.split("\t")[15]
        subm_date_date =  time.strptime(subm_date,"%Y-%m-%d")
        latest_date = time.strptime("2022-05-26","%Y-%m-%d")
        subdates = date.split("-")
        if len(subdates) > 1 and subm_date_date < latest_date:
        #if len(subdates) > 1: 
            year = subdates[0]
            month = subdates[1]
            day = "01"
            if len(subdates) == 3:
                day = subdates[2]
                
            date = year+"-"+month+"-"+day
            date_date =  time.strptime(date,"%Y-%m-%d")
            earliest_date = time.strptime("2021-12-01","%Y-%m-%d")

            if date_date >= earliest_date:
                my = str(year)+"-"+str(month)
                if lineage == "BA.1.1" and subname not in oldseqs:
                    print(line)
                else:
                    month_all[my] += 1
                    if "Omicron" in line:
                        month_omicron[my] += 1

                    if lineage == "BA.1.1":
                        month_ba11[my] += 1
                        reg_b11[region] += 1
                        if "RII" in name:
                            name1 = name.split("/")[2]
                            name2 = name1.split("-")[2]
                            name21 = name2[:-1]
                            name21 = str(name21)
                            if name21 in dorm_ids or "V" in name2:
                                month_dorm[my] += 1
                                reg_b11_dorm[region] += 1

month_dd = defaultdict(int)
reg_dd = defaultdict(int)
with open ("DormDerived_NonDormRusSamples.After16thDec.tsv") as seqs: #list of Russian non-dormitory samples from clade A and their collection dates
    for line in seqs:
        name = line.split("\t")[0]
        subname = name.split("/")[1]
        region = subname.split("-")[0]
        date = line.split("\t")[1]
        subdates = date.split("-")
        #if len(subdates) > 1 and subm_date_date < latest_date:
        if len(subdates) > 1:
            year = subdates[0]
            month = subdates[1]
            day = "01"
            if len(subdates) == 3:
                day = subdates[2]
                my = str(year)+"-"+str(month)
            month_dd[my] += 1
            reg_dd[region] += 1

outf1 = open("Russia_MetadataInTree.Regions.BA11_DormDerived.tsv","a") #table for Fig. 5
outf1.write("region"+"\t"+"BA11"+"\t"+"DormDerived"+"\n")
for region in reg_b11:
    b11 = reg_b11[region]
    dorm = 0
    dd = 0
    if region in reg_b11_dorm:
        dorm = reg_b11_dorm[region]
        b11 = b11 - dorm
    if region in reg_dd:
        dd = reg_dd[region]
    outf1.write(region+"\t"+str(b11)+"\t"+str(dd)+"\n")

sys.exit()


outf = open("Russia_MetadataInTree.Month_All_Omicron_BA11_DormDerived.After1stDec.woDormSamples.tsv","a") #table for Fig. 4A
outf.write("month"+"\t"+"all_seqn"+"\t"+"omicron_seqn"+"\t"+"ba11_seqn"+"\t"+"dd_seqn"+"\n")
for month in month_all:
    all_seqn = month_all[month]
    omicron_seqn = 0
    ba11_seqn = 0
    dd_seqn = 0
    if month in month_omicron:
        omicron_seqn = month_omicron[month]

    if month in month_ba11:
        ba11_seqn = month_ba11[month]

    if month in month_dd:
        dd_seqn = month_dd[month]
        
    if month in month_dorm:
        dorm_seqn = month_dorm[month]
        #print(month+"\t"+str(dorm_seqn))
        all_seqn = all_seqn - dorm_seqn
        omicron_seqn = omicron_seqn - dorm_seqn
        ba11_seqn = ba11_seqn - dorm_seqn

    outf.write(str(month)+"\t"+str(all_seqn)+"\t"+str(omicron_seqn)+"\t"+str(ba11_seqn)+"\t"+str(dd_seqn)+"\n")

outf.close()


