from disk import DataHandler
from multiprocessing import Pool
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import cv2
import argparse
from matplotlib.patches import Circle

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

def anim_h(variable,list_frame):
        data = DataHandler(args.output)
        data_f = data.get(variable).flatten()
        time = data.time
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        H = data.get('H').flatten()
        
        N = int(2*max(H)/50)
        middle = int(N/2)
        space = 500
        dist = 30
        
        
        for j in list_frame:
            temp = np.log10(data.get(variable,time_idx=j))
            h = data.get('H',time_idx=j)
            frame = np.zeros((N,space+dist))
            for i in range(100):
                half_height = int(h[i]/50)
                index = int(space/100)
                frame[middle-half_height:half_height+middle,dist+i*index:dist+index*(i+1)] = temp[i]
            frame[frame==0] = np.nan
            
            frame = np.concatenate((frame[:,::-1],frame),axis=1)
            fig,ax = plt.subplots(1,1,figsize=(40,10))
            circle = Circle( (space+dist,middle), 5, alpha=1,color='black')
            ax.add_patch(circle)
            im = ax.imshow(frame,cmap='turbo',vmin=interval[0],vmax=interval[1])
            cbar = plt.colorbar(im)
            plt.axis('off')
            plt.grid(visible=False)
            
            plt.annotate('%s s'%time[j], (1.05,1.05),fontsize=25, xycoords=cbar.ax.transAxes, va="baseline", ha="center")
            plt.savefig(f'frame_h/frame_%s.png'%(int((j)/4)))
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
                        help="Animation de h")
    parser.add_argument("--trial",
                        action="store_true",
                        help="Animation trial")
    parser.add_argument("--ddd",
                        action="store_true",
                        help="Animation classique")

    args = parser.parse_args()

    # variables de perspective
    rows,cols=400,400
    pt1 = np.float32([[0.5,0],[0,0.5],[1,0.5],[0.5,1]])*400
    pt2 = np.float32([[0.525,0.4],[0.05,0.3],[0.95,0.7],[0.475,0.7]])*400
    matrix = cv2.getPerspectiveTransform(pt1,pt2)
    
    variable = 'TEMP'
    liste_frame = np.arange(1,900,4)
    
    if args.h:
        anim_h(variable,liste_frame)
        
    	
    if args.ddd:
        data = DataHandler(args.output)
        variable = 'TEMP'
        data_f = data.get(variable).flatten()
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]
        anim_3d(liste_frame,interval)
        
    if args.trial:
        data = DataHandler(args.output)
        variable = 'TEMP'
        data_f = data.get(variable).flatten()
        # normalisation de la température
        interval = [np.log10(min(data_f)),np.log10(max(data_f))]

        anim_trial(liste_frame,interval)
