from disk import DataHandler
from multiprocessing import Pool
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import cv2
import argparse
from matplotlib.patches import Circle
from mpl_toolkits.axes_grid1 import make_axes_locatable
import os,gc,sys

# animation 2D -> disque
def anim_trial(list_frame,interval):
    
    for i in list_frame:
        img = np.loadtxt('frame_array/frame_%s.out'%i)
        plt.figure(figsize=(15,15))
        plt.imshow(img,cmap='hot',vmin=interval[0],vmax=interval[1])
        plt.axis('off')
        plt.grid(visible=False)
        cbar = plt.colorbar()
        plt.savefig(f'frame/frame_%s.png'%i)
        plt.close()
    return None

# animation avec perspective -> effet 3D
def anim_3d(list_frame,interval):
    
    for i in list_frame:
        img = np.loadtxt('frame_array/frame_%s.out'%i)
        plt.figure(figsize=(15,15))
        img = cv2.warpPerspective(img,matrix,(cols,rows))
        img[img==0.0] = np.nan
        plt.imshow(img,cmap='hot',vmin=interval[0],vmax=interval[1])
        plt.axis('off')
        plt.grid(visible=False)
        cbar = plt.colorbar()
        plt.savefig(f'frame_3d/frame_%s.png'%i)
        plt.close()
    return None

# animation température-hauteur avec background stellaire
def anim_h(variable,a,b,nb):
        a,b = int(a),int(b)
        pas = int((b-a)/nb)
        list_frame = np.arange(a,b,pas)
        data = DataHandler(args.output)
        print('Nombre de pas de temps : ',len(data.time), 'et pas de temps : ',pas)
        H = data.get('H').flatten()
        VARIABLE = data.get(variable).flatten()
        height=8
        lenght = int(height*4)
        space = 500
        size = len(data.get('H',time_idx=0))
        dist = int(3/100 * space)
        R = int(space/100)
        norm = max(H)*4.5/space
        N = int(space*2/4.5)
        interval = [min(VARIABLE),max(VARIABLE)]
        time = data.time
        middle = int(N/2)
        index = int(space/size)
        print('taille array : ',size,' | dist au trou noir : ',dist,' | rayon trou noir : ', R)
        print('normalisation hauteur : ',norm, ' | interval de T : ',interval)
        print('normalisation espace : ',index)
        star = np.zeros((N,2*space))
        idx = np.array([(i,j) for i in range(N) for j in range(2*space)])
        random_x = np.random.choice(np.arange(N*2*space),size=300,replace=False)
        m_idx = idx[random_x]
        s=1
        for [i,j] in m_idx:
            star[i-s:i+s,j-s:j+s]=interval[1] 
        star[star==0] = np.nan
        plt.style.use('dark_background')
        for j in list_frame:
            temp = data.get(variable,time_idx=j)
            h = data.get('H',time_idx=j)
            frame = np.zeros((N,space))
            for i in range(size):
                half_height = int(h[i]/norm)
                up    = dist+i*index
                down  = dist+index*(i+1)
                frame[middle-half_height:half_height+middle,up:down] = temp[i]
            frame[frame==0] = np.nan
            frame = np.concatenate((frame[:,::-1],frame),axis=1)
            fig,ax = plt.subplots(1,1,figsize=(lenght,height))
            circle = Circle((space,middle), R, alpha=1,color='blue')
            ax.add_patch(circle)
            im = ax.imshow(frame,cmap='hot',vmin=interval[0],vmax=interval[1])
            ax.imshow(star,cmap='hot',vmin=interval[0],vmax=interval[1])
            cbar = plt.colorbar(im,fraction=0.04,aspect=40,pad=0.05,orientation='horizontal')
            cbar.ax.tick_params(labelsize=25)
            plt.axis('off')
            plt.grid(visible=False)
            plt.annotate(f'{time[j]:.3f} s', (-0.2,0),fontsize=30,
             xycoords=cbar.ax.transAxes,va="baseline", ha="center",color='white')
            plt.annotate(r'$\times 10^{7}$'  ,(1.1,0),fontsize=25,
             xycoords=cbar.ax.transAxes,va="baseline", ha="center",color='white')
            plt.savefig(f'frame_h/frame_%s.png'%int((j*nb/(b-a))),dpi=100)
            plt.close()
            
        return None
	
if __name__ == "__main__":
    # Parser
    parser = argparse.ArgumentParser(
        description="Affiche les données obtenues par la simulation.",
        allow_abbrev=True)
    parser.add_argument("output",
                         help="Nom du fichier de sortie (Ex: [data.out])")
    parser.add_argument("--h",
                        action="store_true",
                        help="Animation avec background")
    parser.add_argument("--trial",
                        action="store_true",
                        help="Animation trial")
    parser.add_argument("--ddd",
                        action="store_true",
                        help="Animation classique")
    parser.add_argument('a',help='temps initial')
    parser.add_argument('b',help='temps final')
    args = parser.parse_args()

    # variables de perspective
    rows,cols=400,400
    pt1 = np.float32([[0.5,0],[0,0.5],[1,0.5],[0.5,1]])*400
    pt2 = np.float32([[0.525,0.4],[0.05,0.3],[0.95,0.7],[0.475,0.7]])*400
    matrix = cv2.getPerspectiveTransform(pt1,pt2)
    
    variable = 'TEMP'
    
    # nombre de frame sur l'intervalle de temps
    nb=5
    
    # animation avec background
    if args.h:
        anim_h(variable,args.a,args.b,nb)
        
    # animation 3D
    if args.ddd:
        data = DataHandler(args.output)
        data_f = data.get(variable).flatten()
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        anim_3d(args.a,args.b,interval)
    
    #animation 3D
    if args.trial:
        data = DataHandler(args.output)
        data_f = data.get(variable).flatten()
        # normalisation de la température
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        anim_trial(args.a,args.b,interval)
