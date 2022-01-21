####JOL reactivity bar charts####
##set up
##load libraries
import numpy as np
import matplotlib.pyplot as plt

##forward vs Backward
JOL = [70, 70, 70, 40]
Recall = [65, 35, 45, 15]

##set up the plots
ex1_fig = plt.figure()
ex1_fig.set_size_inches(12,6)

##make the bars
bars1 = JOL
bars2 = Recall

barwidth = 0.25 ##will need to tweak this number

#set bar position 
r1 = np.arange(len(bars1))
r2 = [x + barwidth for x in r1]

##make the sub plots
#fig1
ax1 = ex1_fig.add_subplot(1, 1, 1)

ex1_fig.subplots_adjust(hspace = .45)

rects1 = ax1.bar(r1, bars1, width = barwidth, color = 'dimgrey', edgecolor = 'k',
                label ='JOLs')

rects2 = ax1.bar(r2, bars2, width = barwidth, color = 'silver', edgecolor = 'k',
                label = 'Recall')

##Add labels, legend, and set tick marks
ax1.set_title('Judgments of Learning and Recall Accuracy', fontsize = 16, pad = 15)
ax1.set_ylabel('Mean JOL/ Percent Recall', fontsize = 14)
ax1.set_xlabel('Pair Type', fontsize = 14)
ax1.xaxis.labelpad = 7.5
ax1.set_xticks(r1 + .125) ##get the tick marks centered between the bars
ax1.tick_params(axis = 'x', which = 'major', pad = 2.5) #controls how far labels are from axis
ax1.set_xticklabels(('Forward', 'Backward', 'Symmetrical', 'Unrelated'), fontsize = 12)
box = ax1.get_position()
ax1.set_position([box.x0, box.y0, box.width * 0.8, box.height])
ax1.legend(bbox_to_anchor = (1.04, 0.5), loc = "center left", borderaxespad = 0, fontsize = 12)
ax1.set_ylim([0,100])

##save the thing as a .jpg
ex1_fig.savefig('Sample.jpg', dip = 10000)
