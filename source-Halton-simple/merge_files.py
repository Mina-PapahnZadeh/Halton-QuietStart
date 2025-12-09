import numpy as np

# Python program to 
# demonstrate merging of 
# two files 
n=20480000
#Ul=9.55e-4
#Ut=5.61e-11

x    = []
vx   = []
vy   = []
vz   = []


data1 = open('elec_phase20480000v0.dat')
data2 = open('elec_phase20480000v0=0.dat')
#data1 = open('ion_phase20480000.dat')

for i in range(n):
     temp1 = data1.readline().split()
     x.append(float(temp1[0]))
     vx.append(float(temp1[1]))

for i in range(n):
     temp2 = data2.readline().split()
     vy.append(float(temp2[1]))
#vy=vx
vz=vy


with open('electron_phase20480000-t.dat', 'w') as fh:
    for a,b,c,d in zip(x, vx, vy, vz):
        print("%05.7f  %05.7f  %05.7f %05.7f" % (a, b, c, d), file = fh)
