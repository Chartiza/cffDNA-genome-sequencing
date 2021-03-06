#!/bin/bash

RG='path to genome'
OpenBed='open.bed'
CloseBed='close.bed'

mkdir -p cutadapt
mkdir -p bam

for FQ in ./fastq/file1.fastq ; do
	NN=`basename $FQ`
	echo 'Cut adapters ...'
	cutadapt -a file:/adapters.fa -m 3 -o cutadapt/$NN.cutadapt $NN
 
	echo 'mapping to genome ...'
	bowtie2 -p 8 -x $RG -q fastq/renamed.160419.NP10.fastq.cutadapt | samtools view -bSF4 - > mapped.bam
	samtools sort mapped.bam > $NN.sorted.bam
 
	echo 'Choose reads by regions ...'
	samtools view -b $OpenBed bam/$NN.sorted.bam > bam/$NN.Open.bam
	samtools index bam/$NN.Open.bam
	samtools view -b $CloseBed bam/$NN.sorted.bam > bam/$NN.CLose.bam
	samtools index bam/$NN.CLose.bam

	echo 'Lenght calculation ...'
	samtools view bam/$NN.Open.bam | awk '{print lenght($10)}' | sort -n | uniq -c | perl ...
	samtools view bam/$NN.Open.bam | awk '{print lenght($10)}' | sort -n | uniq -c | perl ...

	echo 'Remoove temp files ...'
	rm ./cutadapt/*
	rm ./bam/*
 
done
