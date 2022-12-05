# This script reads the data stored by program_schema in test.out and test_CN.out

# Import mathematical and plot libraries
import numpy as np
import matplotlib.pyplot as plt

# data
arr =  np.loadtxt("test_temp.out")
print(np.shape(arr), "NT, NX")
NT=np.shape(arr)[0]
abscisse=arr[0]

init=arr[1]
debut=arr[5]
quart=arr[NT//4]
mid=arr[NT//2]
final=arr[-1]

# arr =  np.loadtxt("test_Qm.out")

# initQm=arr[0]
# debutQm=arr[5]
# quartQm=arr[NT//4]
# midQm=arr[NT//2]
# finalQm=arr[-1]

# arr =  np.loadtxt("test_Qp.out")

# initQp=arr[0]
# debutQp=arr[5]
# quartQp=arr[NT//4]
# midQp=arr[NT//2]
# finalQp=arr[-1]

# Setup plot
plt.xlim(np.sqrt(3),10)
plt.xlabel('x')
plt.ylabel('Temperature')
plt.grid()

plt.plot(abscisse, init, color='blueviolet', label='initial')
plt.plot(abscisse, debut, color='lightblue', label=NT//10)
plt.plot(abscisse, quart, color='cornflowerblue', label=NT//4)
plt.plot(abscisse, mid, color='steelblue', label=NT//2)
plt.plot(abscisse, final, color='midnightblue', label=NT)

# plt.plot(abscisse, initQm, color='blueviolet', label='initial')
# plt.plot(abscisse, debutQm, color='lightblue', label=NT//10)
# plt.plot(abscisse, quartQm, color='cornflowerblue', label=NT//4)
# plt.plot(abscisse, midQm, color='steelblue', label=NT//2)
# plt.plot(abscisse, finalQm, color='midnightblue', label=NT)

# plt.plot(abscisse, initQp-initQm, color='black', label='initial')
# plt.plot(abscisse, debutQp-debutQm, color='peachpuff', label=NT//10)
# plt.plot(abscisse, quartQp-quartQm, color='lightsalmon', label=NT//4)
# plt.plot(abscisse, midQp-midQm, color='coral', label=NT//2)
# plt.plot(abscisse, finalQp-finalQm, color='peru', label=NT)


plt.legend()
plt.show()