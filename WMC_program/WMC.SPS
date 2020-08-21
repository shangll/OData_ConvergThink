GET DATA /TYPE=XLS
   /FILE='C:\Users\shangll\Documents\NCKU\WMC\wmcor.xls'
   /SHEET=name 'sub'
   /CELLRANGE=full
   /READNAMES=on
   /ASSUMEDSTRWIDTH=32767.



/*****ordn_m�ܶ����N�q�O�]1�^���ݥ���A��g�X���������ǭn���T�]2�^�S�g���������p�䶶�ǡA��Y���ⶶ�ǿ�**/.

RECODE
  ���� to ���Z  (MISSING=0)  .
EXECUTE .

compute @2_1 = ����+�W��.
compute @2_2 = �G��+����.
compute @2_3 = ��l+�x�n.

compute @3_1 = �~�M+�~��+�ҥ�.
compute @3_2 = �T��+�f��+�ۤ�. 
compute @3_3 = �B�c+�m��+���X.

compute @4_1 = �Ӥ�+�s��+�@��+�d�g.
compute @4_2 = �ƹ�+��î+�j�H+�r��.
compute @4_3 = ����+�O�w+��+���.

compute @5_1 = ���Y+���+����+��v+���.
compute @5_2 = �ƾ�+����+��u+�޳�+�F�P.
compute @5_3 = �U�l+��^+�d�q+�ר�+�F�z.

compute @6_1 = �ͽ�+�ɮ�+�Ѫ�+����+�t��+�֧�.
compute @6_2 = ���}+����+�L��+��+���+�M�l.
compute @6_3 = �j��+���J+�ܤ�+����+���+���Z.



IF (@2_1 ~= 2) sum_2_1 = 0 .
IF (@2_2 ~= 2) sum_2_2 = 0 .
IF (@2_3 ~= 2) sum_2_3 = 0 .

IF (@3_1 ~= 3) sum_3_1 = 0 .
IF (@3_2 ~= 3) sum_3_2 = 0 .
IF (@3_3 ~= 3) sum_3_3 = 0 .

IF (@4_1 ~= 4) sum_4_1 = 0 .
IF (@4_2 ~= 4) sum_4_2 = 0 .
IF (@4_3 ~= 4) sum_4_3 = 0 .

IF (@5_1 ~= 5) sum_5_1 = 0 .
IF (@5_2 ~= 5) sum_5_2 = 0 .
IF (@5_3 ~= 5) sum_5_3 = 0 .

IF (@6_1 ~= 6) sum_6_1 = 0 .
IF (@6_2 ~= 6) sum_6_2 = 0 .
IF (@6_3 ~= 6) sum_6_3 = 0 .



IF (@2_1 = 2) sum_2_1 = 1 .
IF (@2_2 = 2) sum_2_2 = 1 .
IF (@2_3 = 2) sum_2_3 = 1 .

IF (@3_1 = 3) sum_3_1 = 1 .
IF (@3_2 = 3) sum_3_2 = 1 .
IF (@3_3 = 3) sum_3_3 = 1 .

IF (@4_1 = 4) sum_4_1 = 1 .
IF (@4_2 = 4) sum_4_2 = 1 .
IF (@4_3 = 4) sum_4_3 = 1 .

IF (@5_1 = 5) sum_5_1 = 1 .
IF (@5_2 = 5) sum_5_2 = 1 .
IF (@5_3 = 5) sum_5_3 = 1 .

IF (@6_1 = 6) sum_6_1 = 1 .
IF (@6_2 = 6) sum_6_2 = 1 .
IF (@6_3 = 6) sum_6_3 = 1 .



/********���ݥ���]���Ҽ{���Ǫ��`��*******/.
compute total2 = @2_1+@2_2+@2_3.
compute total3 = @3_1+@3_2+@3_3.
compute total4 = @4_1+@4_2+@4_3.
compute total5 = @5_1+@5_2+@5_3.
compute total6 = @6_1+@6_2+@6_3.
compute total = sum(total2,total3,total4,total5,total6).
compute total456 = sum(total4,total5,total6).
compute total56 = sum(total5,total6).

/********����B���Ǥ]�諸�`��*******/.
compute total2_o = @2_1 * ord_2_1 * sum_2_1 + @2_2 * ord_2_2 * sum_2_2 + @2_3 * ord_2_3 * sum_2_3.  
compute total3_o = @3_1 * ord_3_1 * sum_3_1 + @3_2 * ord_3_2 * sum_3_2 + @3_3 * ord_3_3 * sum_3_3.
compute total4_o = @4_1 * ord_4_1 * sum_4_1 + @4_2 * ord_4_2 * sum_4_2 + @4_3 * ord_4_3 * sum_4_3 .  
compute total5_o = @5_1 * ord_5_1 * sum_5_1 + @5_2 * ord_5_2 * sum_5_2 + @5_3 * ord_5_3 * sum_5_3 .  
compute total6_o = @6_1 * ord_6_1 * sum_6_1 + @6_2 * ord_6_2 * sum_6_2 + @6_3 * ord_6_3 * sum_6_3 . 
compute total_o = sum(total2_o,total3_o,total4_o,total5_o,total6_o).
compute total_o456 = sum(total4_o,total5_o,total6_o).
compute total_o56 = sum(total5_o,total6_o).
compute total_o45 = sum(total4_o,total5_o).

DESCRIPTIVES
  VARIABLES=total2 to total_o45
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS .


/********�H�W��WMCall.sav�s�@*********/


/********merge RIF*******/.
get file='I:\My Experiments\others\0yyy\2007integrationexp\WMC\rifall.sav'.
sort cases by �s��.
match FILEs/file=*
  /file='I:\My Experiments\others\0yyy\2007integrationexp\WMC\wmc.sav'
  /BY �s��.
SAVE OUTFILE='I:\My Experiments\others\0yyy\2007integrationexp\WMC\all.sav'.




/********�{���Z�Ū�  merge RIF*******/.

get file='C:\Documents and Settings\chinlan\�ୱ\rif-�{��.sav'.
sort cases by �s��.
match FILEs/file=*
  /file='C:\Documents and Settings\chinlan\�ୱ\wmc-�{��.sav'
  /BY �s��.
SAVE OUTFILE='C:\Documents and Settings\chinlan\�ୱ\�{��-all.sav'.


/********�[�W�Ǹ�*******/.
get file='C:\Documents and Settings\chinlan\�ୱ\�{��-all.sav'.
sort cases by �Ǹ�.
MATCH FILES /FILE=*
 /FILE='C:\Documents and Settings\chinlan\�ୱ\name.sav'
 /BY �Ǹ�.
EXECUTE.

SAVE OUTFILE='C:\Documents and Settings\chinlan\�ୱ\�{��-nameall.sav'.




/********�H�U�O�p��*******/



FREQUENCIES
  VARIABLES=total total_o
  /STATISTICS=STDDEV MEAN MEDIAN SUM
  /ORDER=  ANALYSIS .

T-TEST
  GROUPS = sex(0 1)
  /MISSING = ANALYSIS
  /VARIABLES = total total_o tcorrect rpcorr RPplus RPminus NRP facilite inhibit 
  /CRITERIA = CI(.95) .

SORT CASES BY sex .
SPLIT FILE
  LAYERED BY sex .
CORRELATIONS
  /VARIABLES=tcorrect NRP facilite inhibit
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE .
SPLIT FILE
  OFF.

T-TEST
  GROUPS = sex(0 1)
  /MISSING = ANALYSIS
  /VARIABLES = �־� �Z�� ��c �窫 ���� �г� �a�� ����
  /CRITERIA = CI(.95) .

RECODE
  total
  (Lowest thru 50=1)  (54 thru Highest=3)  (51 thru 53=2)  INTO  total_grp .
VARIABLE LABELS total_grp '�u�@�O�Ф���'.
EXECUTE .

T-TEST
  GROUPS = total_grp(1 3)
  /MISSING = ANALYSIS
  /VARIABLES = total total_o tcorrect rpcorr RPplus RPminus NRP facilite inhibit 
  /CRITERIA = CI(.95) .



FREQUENCIES
  VARIABLES=total total_o
  /ORDER=  ANALYSIS .


FREQUENCIES
  VARIABLES=total total_o
  /STATISTICS=MEAN MEDIAN SUM
  /ORDER=  ANALYSIS .

USE ALL.
COMPUTE filter_$=(�D�� = 2).
VARIABLE LABEL filter_$ '�D�� = 2 (FILTER)'.
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE .


uSE ALL.
COMPUTE filter_$=(�D�� = 4).
VARIABLE LABEL filter_$ '�D�� = 4 (FILTER)'.
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE .
T-TEST
  GROUPS = �D�D(2 3)
  /MISSING = ANALYSIS
  /VARIABLES = rpcorr RPplus RPminus NRP facilite inhibit
  /CRITERIA = CI(.95) .

VARIABLE LABELS rpcorr '����m�ߥ��T�v'.
VARIABLE LABELS �D��  '�����'.
VALUE LABELS  �D��     1  '����@'     2  '����G'     4  '����|'  .

VARIABLE LABELS �D�D  '���籡�p'.
VALUE LABELS  �D�D     1  '�e��'     2  '����'     3  '�x��'  .

VARIABLE LABELS �D�D  '���籡�p'.
VALUE LABELS  �D�D     1  '�e��'     2  '����'.


ONEWAY
  rpcorr RPplus RPminus NRP facilite inhibit BY �D�D
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS
  /POSTHOC = SCHEFFE ALPHA(.05).


GET
  FILE='C:\Documents and Settings\chinlan\�ୱ\all.sav'.

SORT CASES BY �D�� �D�D .
SPLIT FILE
  LAYERED BY �D�� �D�D .


USE ALL.
COMPUTE filter_$=(�D�� = 2).
VARIABLE LABEL filter_$ '�D�� = 2 (FILTER)'.
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE .



T-TEST
  GROUPS = �D�D(1 2)
  /MISSING = ANALYSIS
  /VARIABLES = RPplus NRP
  /CRITERIA = CI(.95) .


SUMMARIZE
  /TABLES=�Ǹ� �m�W total_o total
  /FORMAT=VALIDLIST NOCASENUM TOTAL LIMIT=100
  /TITLE='Case Summaries'
  /MISSING=VARIABLE
  /CELLS=COUNT .

USE ALL.
COMPUTE filter_$=(total  > 0).
VARIABLE LABEL filter_$ 'total  > 0 (FILTER)'.
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE .

DESCRIPTIVES
  VARIABLES=total_o total
  /STATISTICS=MEAN STDDEV MIN MAX .
