import sys
import ete3
from ete3 import Tree
import random
import re
import itertools
from collections import defaultdict
import time

pre_tree = ete3.Tree("uncondensed-final-tree.nh",format=1)
tree = pre_tree&"node_99157"

saved_leaves = list()
human_samples = defaultdict(list)
for leaf in tree:
    ln = leaf.name
    if "dorm" in ln:
        human = ln.split("_")[1]
        human_samples[human].append(ln)
    else:
        saved_leaves.append(ln)


for human in human_samples:
    earliest_date = time.strptime("2023-01-01","%Y-%m-%d")
    samples = human_samples[human]
    print(human+"\t"+str(len(samples)))
    earliest_sample = "none"
    for sample in samples:
        dt = sample.split("|")[2]
        date = time.strptime(dt,"%Y-%m-%d")
        if date < earliest_date:
            earliest_date = date
            earliest_sample = sample
    saved_leaves.append(earliest_sample)

for ll in tree:   
    l = ll.name
    if l not in saved_leaves:
        ll.delete()

tree.write(format=1, outfile="Omicron_26thMayTree.DormDerivedTreeAllSamples.woDuplicates.nwk")
print(str(len(tree)))

