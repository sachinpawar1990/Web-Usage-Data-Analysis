'''

Code used to convert the original dataset to a format for sequence mining
@author: TEAM

'''

import csv
import pandas

df=pandas.read_csv("Case_VRoots.csv", sep=', ', delimiter=",", header='infer')
list_data=[gr['Vid'].tolist() for n, gr in df.groupby('CaseID')]
f = open("list_data.txt", "w")

for lines in list_data:
    f.write(str(lines))
    f.write("\n")
