import numpy as np
import matplotlib.pyplot as plt

T_epais,S_epais=np.loadtxt("results_epais.out",unpack=True)
#T_mince,S_mince=np.loadtxt("results_mince.out",unpack=True)

plt.plot(np.log10(S_epais),np.log10(T_epais),'b-',label="Branche épais")
#plt.loglog(S_mince,T_mince,'r-',label="Branche mince")
plt.title("Courbe en S")
plt.xlabel(r"$log(\Sigma)$")
plt.ylabel("$log(T)$")
plt.show()