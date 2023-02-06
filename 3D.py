from python.disk import DataHandler
from multiprocessing import Pool
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import cv2

def anim_trial(list_frame,interval):
	for i in liste_frame:
		img = np.loadtxt('frame_array/frame_%s.out'%i)
		plt.figure(figsize=(15,15))
		plt.imshow(img,cmap='hot',vmin=interval[0],vmax=interval[1])
		plt.axis('off')
		plt.grid(visible=False)
		plt.colorbar()
		plt.savefig(f'frame/frame_%s.png'%i)
		plt.close()
	return None


def anim_3d(list_frame,interval):
	for i in list_frame:
		img = np.loadtxt('frame_array/frame_%s.out'%i)
		plt.figure(figsize=(15,15))
		img = cv2.warpPerspective(img,matrix,(cols,rows))
		img[img==0.0] = np.nan
		plt.imshow(img,cmap='hot',vmin=interval[0],vmax=interval[1])
		plt.axis('off')
		plt.grid(visible=False)
		plt.colorbar()
		plt.savefig(f'frame_3d/frame_%s.png'%i)
		plt.close()
	return None

def anim_h(list_frame,interval,variable):
		data = DataHandler('data_cycle.out')
		H = data.get('H').flatten()
		N = int(max(H)/10)
		middle = int(N/2)
		for j in list_frame:
			temp = np.log10(data.get(variable,time_idx=j))
			h = data.get('H',time_idx=j)
			frame = np.zeros((N,1000))
			for i in range(100):
				half_height = int(h[i]/20)
				frame[middle:half_height+middle,i*10:10*(i+1)] = temp[i]
				frame[middle-half_height:middle,i*10:10*(i+1)] = temp[i]
			frame[frame==0] = np.nan
			plt.imshow(frame,cmap='coolwarm',vmin=interval[0],vmax=interval[1])
			plt.axis('off')
			plt.grid(visible=False)
			plt.colorbar()
			plt.savefig(f'frame_h/frame_%s.png'%j)
			plt.close()
		return None
		
		
data = DataHandler('data_cycle.out')
variable = 'TEMP'
data_f = data.get(variable).flatten()
# list des temps 
liste_frame = np.arange(350,700)
# normalisation de la température
interval = [np.log10(min(data_f)),np.log10(max(data_f))]

# variables de perspective
rows,cols=400,400
pt1 = np.float32([[0.5,0],[0,0.5],[1,0.5],[0.5,1]])*400
pt2 = np.float32([[0.525,0.4],[0.05,0.3],[0.95,0.7],[0.475,0.7]])*400
matrix = cv2.getPerspectiveTransform(pt1,pt2)


anim_h(variable,liste_frame,interval)
#anim_3d(liste_frame,interval)
#anim_trial(liste_frame,interval)

