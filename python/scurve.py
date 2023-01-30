import numpy as np
import matplotlib.pyplot as plt

T_epais,S_epais,positions=np.loadtxt("./../output/results_epais.out",unpack=True,usecols=(0,1,2))
T_mince,S_mince,positions2=np.loadtxt("./../output/results_mince.out",unpack=True,usecols=(0,1,2))

p=[]
for i in positions : 
    if i not in p: 
        p.append(i) 


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
    plt.plot(np.log10(S_epais2),np.log10(T_epais2),'bo',label="Branche épais")
    plt.plot(np.log10(S_mince2),np.log10(T_mince2),'ro',label="Branche mince")
    plt.title("Courbe en S à x=%f"%(p[i]))
    plt.xlabel(r"$log(\Sigma)$")
    plt.ylabel("$log(T)$")
    plt.legend()
    plt.xlim([2,3.5])
    plt.ylim([5.5,7.6])
    plt.show()