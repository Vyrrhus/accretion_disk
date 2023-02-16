import numpy as np
import matplotlib.pyplot as plt

T_epais,S_epais,r,positions=np.loadtxt("./../output/scurve/epais.out",unpack=True,usecols=(0,1,2,3))
T_mince,S_mince,r2,positions2=np.loadtxt("./../output/scurve/mince.out",unpack=True,usecols=(0,1,2,3))

p=[]
r_plot=[]
for i in positions : 
    if i not in p: 
        p.append(i)
for j in r:
    if j not in r_plot:
        r_plot.append(j)

for i in range(0,len(p)):
    T_epais2=[]
    S_epais2=[]
    T_mince2=[]
    S_mince2=[]
    for j in range(0,len(positions)):
        if positions[j]==p[i]:
            T_epais2.append(T_epais[j])
            S_epais2.append(S_epais[j])
    for j in range(0,len(positions2)):
        if positions2[j]==p[i]:
            T_mince2.append(T_mince[j])
            S_mince2.append(S_mince[j])
            
    S_epais2=np.array(S_epais2)
    S_mince2=np.array(S_mince2)            

    plt.plot(np.log10(S_epais2*1e-1),np.log10(T_epais2),'bo',label="Branche épaisse")
    plt.plot(np.log10(S_mince2*1e-1),np.log10(T_mince2),'ro',label="Branche mince")
    plt.title("Courbe en S")
    plt.xlabel(r"$log(\Sigma)[g/cm^2]$")
    plt.ylabel("$log(T)[K]$")
    plt.legend()
    plt.xlim([2,3.5])
    plt.ylim([6.5,7.6])
    plt.show()