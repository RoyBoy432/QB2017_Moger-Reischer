from __future__ import division
import math

def ar(N, alpha, r):
    thuggy=0
    thuggy=N*alpha*(1-alpha)**(r-1)
    return thuggy
    
testlist=[]
for i in range(0,20):
    testlist.append(math.log(ar(448, .3, i)))

print (testlist)
print (testlist[0]-testlist[1])
#0.35667494393873245
print(testlist[1]-testlist[2])
#0.35667494393873245
