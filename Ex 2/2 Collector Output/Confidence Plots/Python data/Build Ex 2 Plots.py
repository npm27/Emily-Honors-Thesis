import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat_overall = pd.read_csv("all.csv")

#Get IS
dat = dat_overall[dat_overall['Encoding'] == 'IS']

dat['diff'] = dat['Upper'].sub(dat['Lower'])
dat['diff2'] = dat['diff'].div(2)

##make subsets
datF = dat[dat['Direction'] == 'F']
datB = dat[dat['Direction'] == 'B']
datS = dat[dat['Direction'] == 'S']
datU = dat[dat['Direction'] == 'U']

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(11,8)

ax1 = fig.add_subplot(2, 2, 1)
ax2 = fig.add_subplot(2, 2, 2)
ax3 = fig.add_subplot(2, 2, 3)
ax4 = fig.add_subplot(2, 2, 4)

dot_line = np.arange(100)

major_ticks = np.arange(0, 101, 20)

fig.text(0.5, 0.04, 'JOL Rating', ha='center', fontsize=18)
fig.text(0.04, 0.5, '% Correct Recall', va='center', rotation='vertical', fontsize=18)

##forward
x1 = datF.JOL_Bin.values
y1 = datF.Average.values

ax1.plot(dot_line, 'k--')
ax1.plot(x1, y1, marker = '.', color = 'k')

ax1.set_xticks(major_ticks)
ax1.set_yticks(major_ticks)

ax1.set_title("Forward", fontsize = 16)

ax1.errorbar(x1, y1, yerr=(datF['diff2']), fmt='none', c= 'k', capsize=5)

##backward
x2 = datB.JOL_Bin.values
y2 = datB.Average.values

ax2.plot(dot_line, 'k--')
ax2.plot(x2, y2, marker = '.', color = 'k')

ax2.set_xticks(major_ticks)
ax2.set_yticks(major_ticks)

ax2.set_title("Backward", fontsize = 16)

ax2.errorbar(x2, y2, yerr=(datB['diff2']), fmt='none', c= 'k', capsize=5)

##symmetrical
x3 = datS.JOL_Bin.values
y3 = datS.Average.values

ax3.plot(dot_line, 'k--')
ax3.plot(x3, y3, marker = '.', color = 'k')

ax3.set_xticks(major_ticks)
ax3.set_yticks(major_ticks)

ax3.set_title("Symmetrical", fontsize = 16)

ax3.errorbar(x3, y3, yerr=(datS['diff2']), fmt='none', c= 'k', capsize=5)

##unrelated
x4 = datU.JOL_Bin.values
y4 = datU.Average.values

ax4.plot(dot_line, 'k--')
ax4.plot(x4, y4, marker = '.', color = 'k')

ax4.set_xticks(major_ticks)
ax4.set_yticks(major_ticks)

ax4.set_title("Unrelated", fontsize = 16)

ax4.errorbar(x4, y4, yerr=(datU['diff2']), fmt='none', c= 'k', capsize=5)

##save figure

fig.suptitle('Item-Specific Group', fontsize=20)

fig
fig.savefig('Ex2_IS2.png')

#Get RL
dat = dat_overall[dat_overall['Encoding'] == 'RL']

dat['diff'] = dat['Upper'].sub(dat['Lower'])
dat['diff2'] = dat['diff'].div(2)

##make subsets
datF = dat[dat['Direction'] == 'F']
datB = dat[dat['Direction'] == 'B']
datS = dat[dat['Direction'] == 'S']
datU = dat[dat['Direction'] == 'U']

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(11,8)

ax1 = fig.add_subplot(2, 2, 1)
ax2 = fig.add_subplot(2, 2, 2)
ax3 = fig.add_subplot(2, 2, 3)
ax4 = fig.add_subplot(2, 2, 4)

dot_line = np.arange(100)

major_ticks = np.arange(0, 101, 20)

fig.text(0.5, 0.04, 'JOL Rating', ha='center', fontsize=18)
fig.text(0.04, 0.5, '% Correct Recall', va='center', rotation='vertical', fontsize=18)

##forward
x1 = datF.JOL_Bin.values
y1 = datF.Average.values

ax1.plot(dot_line, 'k--')
ax1.plot(x1, y1, marker = '.', color = 'k')

ax1.set_xticks(major_ticks)
ax1.set_yticks(major_ticks)

ax1.set_title("Forward", fontsize = 16)

ax1.errorbar(x1, y1, yerr=(datF['diff2']), fmt='none', c= 'k', capsize=5)

##backward
x2 = datB.JOL_Bin.values
y2 = datB.Average.values

ax2.plot(dot_line, 'k--')
ax2.plot(x2, y2, marker = '.', color = 'k')

ax2.set_xticks(major_ticks)
ax2.set_yticks(major_ticks)

ax2.set_title("Backward", fontsize = 16)

ax2.errorbar(x2, y2, yerr=(datB['diff2']), fmt='none', c= 'k', capsize=5)

##symmetrical
x3 = datS.JOL_Bin.values
y3 = datS.Average.values

ax3.plot(dot_line, 'k--')
ax3.plot(x3, y3, marker = '.', color = 'k')

ax3.set_xticks(major_ticks)
ax3.set_yticks(major_ticks)

ax3.set_title("Symmetrical", fontsize = 16)

ax3.errorbar(x3, y3, yerr=(datS['diff2']), fmt='none', c= 'k', capsize=5)

##unrelated
x4 = datU.JOL_Bin.values
y4 = datU.Average.values

ax4.plot(dot_line, 'k--')
ax4.plot(x4, y4, marker = '.', color = 'k')

ax4.set_xticks(major_ticks)
ax4.set_yticks(major_ticks)

ax4.set_title("Unrelated", fontsize = 16)

ax4.errorbar(x4, y4, yerr=(datU['diff2']), fmt='none', c= 'k', capsize=5)

##save figure

fig.suptitle('Relational Group', fontsize=20)

fig
fig.savefig('Ex2_RL2.png')

#Get READ
dat = dat_overall[dat_overall['Encoding'] == 'READ']

dat['diff'] = dat['Upper'].sub(dat['Lower'])
dat['diff2'] = dat['diff'].div(2)

##make subsets
datF = dat[dat['Direction'] == 'F']
datB = dat[dat['Direction'] == 'B']
datS = dat[dat['Direction'] == 'S']
datU = dat[dat['Direction'] == 'U']

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(11,8)

ax1 = fig.add_subplot(2, 2, 1)
ax2 = fig.add_subplot(2, 2, 2)
ax3 = fig.add_subplot(2, 2, 3)
ax4 = fig.add_subplot(2, 2, 4)

dot_line = np.arange(100)

major_ticks = np.arange(0, 101, 20)

fig.text(0.5, 0.04, 'JOL Rating', ha='center', fontsize=18)
fig.text(0.04, 0.5, '% Correct Recall', va='center', rotation='vertical', fontsize=18)

##forward
x1 = datF.JOL_Bin.values
y1 = datF.Average.values

ax1.plot(dot_line, 'k--')
ax1.plot(x1, y1, marker = '.', color = 'k')

ax1.set_xticks(major_ticks)
ax1.set_yticks(major_ticks)

ax1.set_title("Forward", fontsize = 16)

ax1.errorbar(x1, y1, yerr=(datF['diff2']), fmt='none', c= 'k', capsize=5)

##backward
x2 = datB.JOL_Bin.values
y2 = datB.Average.values

ax2.plot(dot_line, 'k--')
ax2.plot(x2, y2, marker = '.', color = 'k')

ax2.set_xticks(major_ticks)
ax2.set_yticks(major_ticks)

ax2.set_title("Backward", fontsize = 16)

ax2.errorbar(x2, y2, yerr=(datB['diff2']), fmt='none', c= 'k', capsize=5)

##symmetrical
x3 = datS.JOL_Bin.values
y3 = datS.Average.values

ax3.plot(dot_line, 'k--')
ax3.plot(x3, y3, marker = '.', color = 'k')

ax3.set_xticks(major_ticks)
ax3.set_yticks(major_ticks)

ax3.set_title("Symmetrical", fontsize = 16)

ax3.errorbar(x3, y3, yerr=(datS['diff2']), fmt='none', c= 'k', capsize=5)

##unrelated
x4 = datU.JOL_Bin.values
y4 = datU.Average.values

ax4.plot(dot_line, 'k--')
ax4.plot(x4, y4, marker = '.', color = 'k')

ax4.set_xticks(major_ticks)
ax4.set_yticks(major_ticks)

ax4.set_title("Unrelated", fontsize = 16)

ax4.errorbar(x4, y4, yerr=(datU['diff2']), fmt='none', c= 'k', capsize=5)

fig.suptitle('Read Group', fontsize=20)

##save figure
fig
fig.savefig('Ex2_READ2.png')