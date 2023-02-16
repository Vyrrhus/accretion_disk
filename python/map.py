import matplotlib.pyplot as plt
import numpy as np

n=100

T, S, QpmQm, x_ad, r=np.loadtxt("./../output/scurve/coupe.out", unpack=True)

liste_T=[]
liste_x_ad=[]
liste_r=[]

for i in range(0,len(T)):
    if T[i] not in liste_T:
        liste_T.append(T[i])
    if x_ad[i] not in liste_x_ad:
        liste_x_ad.append(x_ad[i])
    if r[i] not in liste_r:
        liste_r.append(r[i])

z=np.zeros(len(liste_T))

for i in range(0,len(liste_x_ad)):
   plt.plot(np.log10(liste_T),QpmQm[i*n:(i+1)*n],'b-')
   plt.plot(np.log10(liste_T),z,'k-')
   plt.title(r"Coupe de $Q_+-Q_-$ à r=%f cm"%(liste_r[i]*1e2))
   plt.xlabel("T[K]")
   plt.ylabel(r"$Q_+-Q_-[J/s/m^2]$")
   plt.ylim([np.min(QpmQm[i*n:(i+1)*n]),-np.min(QpmQm[i*n:(i+1)*n])])
   plt.show()