from disk import DataHandler
from multiprocessing import Pool
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import cv2
import argparse
from matplotlib.patches import Circle
import os,gc

# animation 2D -> disque
def anim_trial(list_frame,interval):
    
    for i in liste_frame:
        img = np.loadtxt('frame_array/frame_%s.out'%i)
        plt.figure(figsize=(15,15))
        plt.imshow(img,cmap='hot',vmin=interval[0],vmax=interval[1])
        plt.axis('off')
        plt.grid(visible=False)
        cbar = plt.colorbar()
        plt.annotate('%s s'%time[i], (1.05,1.05), xycoords=cbar.ax.transAxes, va="baseline", ha="center") 
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
        plt.annotate('%s s'%time[i], (1.05,1.05), xycoords=cbar.ax.transAxes, va="baseline", ha="center")
        plt.savefig(f'frame_3d/frame_%s.png'%i)
        plt.close()
    return None

# animation température-hauteur avec background stellaire
def anim_h(variable,a,b,nb):
        a,b = int(a),int(b)
        list_frame = np.arange(a,b,int((b-a)/nb))
        #data_f = data.get(variable).flatten()
        time = data.time
        middle = int(N/2)
        index = int(space/100)
        
        # ouvre les données et set les constantes d'image 2D
        data = DataHandler(args.output)
        H = data.get('H').flatten()
        space = 500
        dist = int(3/100 * space)
        norm = 2*max(H)*4.6/2/(space+dist)
        N = int((space+dist)*2/4.6)
        interval = [3.0e5,2.5e7]
    
        # créer le background stellaire
        star = np.zeros((N,2*(space+dist)))
        idx = np.array([(i,j) for i in range(N) for j in range(2*(space+dist))])
        random_x = np.random.choice(np.arange(N*2*(space+dist)),size=50000,replace=False)
        m_idx = idx[random_x]
        for [i,j] in m_idx:
	     star[i,j]=interval[1]
        star[star==0] = np.nan
        
        plt.style.use('dark_background')
        
        for j in list_frame:
            
            # récupère données de temp à chaque pas de temps
            temp = data.get(variable,time_idx=j)
            # récupère données de demi-hauteur à chaque pas de temps
            h = data.get('H',time_idx=j)
            # initialiser a matrice 2D
            frame = np.zeros((N,space+dist))
            # associe à chaque valeur de h une valeur de température pour créer l'epaisseur
            for i in range(100):
                half_height = int(h[i]/norm)
                frame[middle-half_height:half_height+middle,dist+i*index:dist+index*(i+1)] = temp[i]
            frame[frame==0] = np.nan
            frame = np.concatenate((frame[:,::-1],frame),axis=1)
            fig,ax = plt.subplots(1,1,figsize=(50,20))
            # le trou noir
            circle = Circle( (space+dist,middle), dist/3, alpha=1,color='blue')
            ax.add_patch(circle)
            # affichage
            im = ax.imshow(frame,cmap='hot',vmin=interval[0],vmax=interval[1])
            ax.imshow(star,cmap='hot',vmin=interval[0],vmax=interval[1])
            
            # paramètres d'affichage
            cbar = plt.colorbar(im,shrink=0.6)
            cbar.ax.tick_params(labelsize=40)
            plt.axis('off')
            plt.grid(visible=False)
            plt.annotate('%s s'%time[j], (1.3,1.3),
                        fontsize=40, xycoords=cbar.ax.transAxes,
                        va="baseline", ha="center",color='white')
            plt.annotate('1e7',(1.05,1.05),fontsize=40, xycoords=cbar.ax.transAxes,
                        va="baseline", ha="center",color='white')
                        
            # enregistrement frame
            plt.savefig(f'frame_h/frame_%s.png'%int((j*nb/(b-a))),dpi=50)
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
    nb=300
    
    # animation avec background
    if args.h:
        anim_h(variable,args.a,args.b,nb)
        
    # animation 3D
    if args.ddd:
        data = DataHandler(args.output)
        data_f = data.get(variable).flatten()
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        anim_3d(liste_frame,interval)
    
    #animation 3D
    if args.trial:
        data = DataHandler(args.output)
        data_f = data.get(variable).flatten()
        # normalisation de la température
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        anim_trial(liste_frame,interval)
