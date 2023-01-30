import matplotlib.pyplot as plt
import numpy as np

n=100

T, S, QpmQm, x_ad=np.loadtxt("./../output/map.out", unpack=True)

liste_T=[]
liste_x_ad=[]

for i in range(0,len(T)):
    if T[i] not in liste_T:
        liste_T.append(T[i])
    if x_ad[i] not in liste_x_ad:
        liste_x_ad.append(x_ad[i])

z=np.zeros(len(liste_T))

for i in range(0,len(liste_x_ad)):
   plt.plot(np.log10(liste_T),QpmQm[i*n:(i+1)*n],'b-')
   plt.plot(np.log10(liste_T),z,'k-')
   plt.title("x=%f et Sigma=%f"%(liste_x_ad[i],S[n*i]))
   plt.xlabel("T")
   plt.ylabel("QpmQm")
   plt.ylim([np.min(QpmQm[i*n:(i+1)*n]),-np.min(QpmQm[i*n:(i+1)*n])])
   plt.show()