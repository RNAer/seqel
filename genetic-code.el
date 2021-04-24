;;; genetic-code.el -- store tables of genetic code  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; Keywords: DNA, RNA, protein
;; License: BSD-3

;;; Commentary:
;;  * The translation tables are downloaded from
;;    http://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi


;;; Code:


(defun get-translation-table (n)
  "Return tranlation table.

The tables are downloaded from http://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi"
  (cond ((eq n 1) ; standard genetic code
         ;; 1st: codon
         ;; 2nd: amino acid
         ;; 3rd: start codon
         '(("TTT" ?F -)
           ("TTC" ?F -)
           ("TTA" ?L -)
           ("TTG" ?L M)
           ("TCT" ?S -)
           ("TCC" ?S -)
           ("TCA" ?S -)
           ("TCG" ?S -)
           ("TAT" ?Y -)
           ("TAC" ?Y -)
           ("TAA" ?* -) ; stop codon
           ("TAG" ?* -)
           ("TGT" ?C -)
           ("TGC" ?C -)
           ("TGA" ?* -)
           ("TGG" ?W -)
           ("CTT" ?L -)
           ("CTC" ?L -)
           ("CTA" ?L -)
           ("CTG" ?L M)
           ("CCT" ?P -)
           ("CCC" ?P -)
           ("CCA" ?P -)
           ("CCG" ?P -)
           ("CAT" ?H -)
           ("CAC" ?H -)
           ("CAA" ?Q -)
           ("CAG" ?Q -)
           ("CGT" ?R -)
           ("CGC" ?R -)
           ("CGA" ?R -)
           ("CGG" ?R -)
           ("ATT" ?I -)
           ("ATC" ?I -)
           ("ATA" ?I -)
           ("ATG" ?M M)
           ("ACT" ?T -)
           ("ACC" ?T -)
           ("ACA" ?T -)
           ("ACG" ?T -)
           ("AAT" ?N -)
           ("AAC" ?N -)
           ("AAA" ?K -)
           ("AAG" ?K -)
           ("AGT" ?S -)
           ("AGC" ?S -)
           ("AGA" ?R -)
           ("AGG" ?R -)
           ("GTT" ?V -)
           ("GTC" ?V -)
           ("GTA" ?V -)
           ("GTG" ?V -)
           ("GCT" ?A -)
           ("GCC" ?A -)
           ("GCA" ?A -)
           ("GCG" ?A -)
           ("GAT" ?D -)
           ("GAC" ?D -)
           ("GAA" ?E -)
           ("GAG" ?E -)
           ("GGT" ?G -)
           ("GGC" ?G -)
           ("GGA" ?G -)
           ("GGG" ?G -)))
        ((eq n 2) ; Vertebrate Mitochondrial Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?-)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?W ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?M)
           ("ATC" ?I ?M)
           ("ATA" ?M ?M)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?* ?-)
           ("AGG" ?* ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?M)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ((eq n 3) ; Yeast Mitochondrial Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?-)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?W ?-)
           ("TGG" ?W ?-)
           ("CTT" ?T ?-)
           ("CTC" ?T ?-)
           ("CTA" ?T ?-)
           ("CTG" ?T ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?-)
           ("ATC" ?I ?-)
           ("ATA" ?M ?M)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?R ?-)
           ("AGG" ?R ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?-)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ;; Mold, Protozoan, and Coelenterate Mitochondrial Code and
        ;; the Mycoplasma/Spiroplasma Code
        ((eq n 4)
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?M)
           ("TTG" ?L ?M)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?W ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?M)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?M)
           ("ATC" ?I ?M)
           ("ATA" ?I ?M)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?R ?-)
           ("AGG" ?R ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?M)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ((eq n 5)
         ;; Invertebrate Mitochondrial Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?M)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?W ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?M)
           ("ATC" ?I ?M)
           ("ATA" ?M ?M)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?S ?-)
           ("AGG" ?S ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?M)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ((eq n 6)
         ;; Ciliate, Dasycladacean and Hexamita Nuclear Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?-)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?Q ?-)
           ("TAG" ?Q ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?* ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?-)
           ("ATC" ?I ?-)
           ("ATA" ?I ?-)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?R ?-)
           ("AGG" ?R ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?-)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ((eq n 9)
         ;; The Echinoderm and Flatworm Mitochondrial Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?-)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?W ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?-)
           ("ATC" ?I ?-)
           ("ATA" ?I ?-)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?N ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?S ?-)
           ("AGG" ?S ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?M)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))
        ((eq n 10)
         ;; The Euplotid Nuclear Code
         '(("TTT" ?F ?-)
           ("TTC" ?F ?-)
           ("TTA" ?L ?-)
           ("TTG" ?L ?-)
           ("TCT" ?S ?-)
           ("TCC" ?S ?-)
           ("TCA" ?S ?-)
           ("TCG" ?S ?-)
           ("TAT" ?Y ?-)
           ("TAC" ?Y ?-)
           ("TAA" ?* ?-)
           ("TAG" ?* ?-)
           ("TGT" ?C ?-)
           ("TGC" ?C ?-)
           ("TGA" ?C ?-)
           ("TGG" ?W ?-)
           ("CTT" ?L ?-)
           ("CTC" ?L ?-)
           ("CTA" ?L ?-)
           ("CTG" ?L ?-)
           ("CCT" ?P ?-)
           ("CCC" ?P ?-)
           ("CCA" ?P ?-)
           ("CCG" ?P ?-)
           ("CAT" ?H ?-)
           ("CAC" ?H ?-)
           ("CAA" ?Q ?-)
           ("CAG" ?Q ?-)
           ("CGT" ?R ?-)
           ("CGC" ?R ?-)
           ("CGA" ?R ?-)
           ("CGG" ?R ?-)
           ("ATT" ?I ?-)
           ("ATC" ?I ?-)
           ("ATA" ?I ?-)
           ("ATG" ?M ?M)
           ("ACT" ?T ?-)
           ("ACC" ?T ?-)
           ("ACA" ?T ?-)
           ("ACG" ?T ?-)
           ("AAT" ?N ?-)
           ("AAC" ?N ?-)
           ("AAA" ?K ?-)
           ("AAG" ?K ?-)
           ("AGT" ?S ?-)
           ("AGC" ?S ?-)
           ("AGA" ?R ?-)
           ("AGG" ?R ?-)
           ("GTT" ?V ?-)
           ("GTC" ?V ?-)
           ("GTA" ?V ?-)
           ("GTG" ?V ?-)
           ("GCT" ?A ?-)
           ("GCC" ?A ?-)
           ("GCA" ?A ?-)
           ("GCG" ?A ?-)
           ("GAT" ?D ?-)
           ("GAC" ?D ?-)
           ("GAA" ?E ?-)
           ("GAG" ?E ?-)
           ("GGT" ?G ?-)
           ("GGC" ?G ?-)
           ("GGA" ?G ?-)
           ("GGG" ?G ?-)))))
  (provide 'genetic-code)
