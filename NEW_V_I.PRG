*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            ����� �牀������ �牀甃�矗� ぅ筌�皚諷 �����き諷.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE New_V_I

PRIVATE sch, mss

sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)

IF .NOT. FILE_O(log_path+"N_T_I.DBF")   && �モ ����� � 爛с�赳�皰�
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"蹍▲爲� �����き諷 �� �牀├〓え��瘡."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_I")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ぅ筌�皚諷 �����き諷!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_I

PRIVATE ln, wd, tt, ft
DIMENSION tt(3), ft(1)

IF SET("CENTURY") = "ON"
  tt(1) = ;
  "陳陳陳賃陳賃陳陳陳陳陳賃陳陳陳陳陳堕陳陳陳陳陳陳陳賃陳�"
  tt(2) = ;
  " N/��� ��爼� N ���-��  �   ����   �   ��┘��       ���."
  tt(3) = ;
  "陳陳陳津陳津陳陳陳陳陳津陳陳陳陳陳田陳陳陳陳陳陳陳津陳�"
  ft(1) = ;
  "陳陳陳珍陳珍陳陳陳陳陳珍陳陳陳陳陳祖陳陳陳陳陳陳陳珍陳�"
ELSE
  tt(1) = ;
  "陳陳陳賃陳賃陳陳陳陳陳賃陳陳陳陳堕陳陳陳陳陳陳陳賃陳�"
  tt(2) = ;
  " N/��� ��爼� N ���-��  �  ����  �   ��┘��       ���."
  tt(3) = ;
  "陳陳陳津陳津陳陳陳陳陳津陳陳陳陳田陳陳陳陳陳陳陳津陳�"
  ft(1) = ;
  "陳陳陳珍陳珍陳陳陳陳陳珍陳陳陳陳祖陳陳陳陳陳陳陳珍陳�"
ENDIF

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_I_M", "Tst_I_E", "Tst_I_I", ln, wd, ;
                "���甌� ぅ筌�皚諷 �����き諷", tt, ft

@ 1, 1 SAY "�"
@ 2, 1 SAY "�"
@ 3, 1 SAY "�"
@ WROWS()-1, 1 SAY "�"

@ 1, WCOLS()-2 SAY "�"
@ 2, WCOLS()-2 SAY "�"
@ 3, WCOLS()-2 SAY "�"
@ WROWS()-1, WCOLS()-2 SAY "�"

ACTIVATE WINDOW Tst_I_E
GO TOP
BROWSE FIELDS NUMB:H="",     ;
              FIRM:H="",     ;
              DOC_NUM:H="",  ;
              DOC_DATE:H="", ;
              DOC_NAME:H="", ;
              IS_DEF:H="",   ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              NOMENU WINDOW Tst_I_I IN WINDOW Tst_I_E COLOR (sch)

RELEASE WINDOW Tst_I_I
RELEASE WINDOW Tst_I_M
RELEASE WINDOW Tst_I_E
CLOSE DATABASES
RETURN
