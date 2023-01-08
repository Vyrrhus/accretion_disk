import numpy as np
import matplotlib.pyplot as plt

T_epais,S_epais=np.loadtxt("./../results_epais.out",unpack=True,usecols=(0,1))
#T_mince,S_mince=np.loadtxt("./../results_mince.out",unpack=True,usecols=(0,1))

plt.plot(np.log10(S_epais[0:80]),np.log10(T_epais[0:80]),'b-',label="Branche épais")
#plt.plot(np.log10(S_mince),np.log10(T_mince),'r-',label="Branche mince")
plt.title("Courbe en S")
plt.xlabel(r"$log(\Sigma)$")
plt.ylabel("$log(T)$")
plt.legend()
plt.xlim([2.4,3.2])
plt.ylim([6.6,7.6])
plt.show()