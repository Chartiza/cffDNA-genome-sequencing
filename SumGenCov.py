#python ./unite.py ./dir_with_files/
import sys
import os

dir = sys.argv[1]
files = os.listdir(dir)

len_smpl={}
samples=[]

for count in range(3,221):
	len_smpl[count]=[]
for file in files:
	if 'open.txt' in file:
		est=set()
		samples.append(file)
		for l in open(dir+file):
			data=l.rstrip().split()
			len_smpl[int(data[1])].append(data[0])
			est.add(int(data[1]))
		for count in range(3,221):
			if count not in est:
				len_smpl[count].append('0')
with open('SumTable_Open.tsv','w') as f:
	f.write('len'+'\t'+'\t'.join(samples)+'\n')
	for count in range(3,221):
		f.write(str(count)+'\t'+'\t'.join(len_smpl[count])+'\n')
