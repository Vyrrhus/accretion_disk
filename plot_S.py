# This script reads the data stored by program_schema in test.out and test_CN.out

# Import mathematical and plot libraries
import numpy as np
import matplotlib.pyplot as plt

# data
arr =  np.loadtxt("test_cn.out")
print(np.shape(arr), "NT, NX")
NT=np.shape(arr)[0]
abscisse=arr[0]

init_CN=arr[1]
debut_CN=arr[5]
quart_CN=arr[NT//4]
mid_CN=arr[NT//2]
final_CN=arr[-1]


arr =  np.loadtxt("test.out")
init=arr[1]
debut=arr[5]
quart=arr[NT//4]
mid=arr[NT//2]
final=arr[-1]

#calcul d'erreurs
erreur=np.zeros(5)
erreur[0]=np.max(abs(init-init_CN)/init)
erreur[1]=np.max(abs(debut-debut_CN)/debut)
erreur[2]=np.max(abs(quart-quart_CN)/quart)
erreur[3]=np.max(abs(mid-mid_CN)/mid)
erreur[4]=np.max(abs(final-final_CN)/final)

print('erreur:', erreur)



# Setup plot
plt.xlim(np.sqrt(3),10)
plt.xlabel('x')
plt.ylabel('S')
plt.grid()

plt.plot(abscisse, init_CN, color='blueviolet', label='initial')
plt.plot(abscisse, debut_CN, color='peachpuff', label=5)
plt.plot(abscisse, quart_CN, color='lightsalmon', label=NT//4)
plt.plot(abscisse, mid_CN, color='coral', label=NT//2)
plt.plot(abscisse, final_CN, color='peru', label=NT)

plt.plot(abscisse, init, color='blueviolet', label='initial')
plt.plot(abscisse, debut, color='lightblue', label=5)
plt.plot(abscisse, quart, color='cornflowerblue', label=NT//4)
plt.plot(abscisse, mid, color='steelblue', label=NT//2)
plt.plot(abscisse, final, color='midnightblue', label=NT)


plt.legend()
plt.show()


