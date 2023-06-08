import sys
import ete3
from ete3 import Tree
import random
import re
import random
from datetime import date

tree = ete3.Tree("BA11.FULL.uncondensed-final-tree.nh",format=1)
threshold_date = date(2021,12,30)

dorm_leaves = list()
rus_leaves = list()
nonrus_leaves = list()
for ll in tree:   
    l = ll.name
        #print (l)
    if "dorm" in l:
        dorm_leaves.append(l)
    elif "Russia" in l:
        new_date = l.split("|")[1]
        subdates = new_date.split("-")
        if len(subdates) == 3:
            y,m,d = [int(x) for x in new_date.split("-")]
            sample_date = date(y,m,d)
            if sample_date < threshold_date:
                rus_leaves.append(l)     
    else:
        new_date = l.split("|")[1]
        subdates = new_date.split("-")
        if len(subdates) == 3:
            y,m,d = [int(x) for x in new_date.split("-")]
            sample_date = date(y,m,d)
            if sample_date < threshold_date:
                nonrus_leaves.append(l)
        
random.shuffle(nonrus_leaves)
remain_nonrus = 400
leaves1 = nonrus_leaves[:remain_nonrus]

new_leaves = leaves1+leaves2+leaves3
tree.prune(new_leaves, preserve_branch_length=True)
tree.write(format=1, outfile="BA11.OmicronReference.113Dorm.AllRus400Nonrus.Before16thDec.newick")

