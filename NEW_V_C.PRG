*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍甃�矗 ぅ筌�皚諷 ぅ��Ж皰�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE New_V_C

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

IF .NOT. FILE_O(log_path+"N_T_C.DBF")   && �モ ����� � 爛с�赳�皰�
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"蹍▲爲� ぅ��Ж皰� �� �牀├〓え��瘡."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_C")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ぅ筌�皚諷 ぅ��Ж皰�!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_C

PRIVATE ln, wd, tt, ft, w1
DIMENSION tt(3), ft(1)
tt(1) = ;
"陳陳陳賃陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳堕陳�"
tt(2) = ;
" N/��� �         ���祚�皀��                               ��ヤ."
tt(3) = ;
"陳陳陳津陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳田陳�"
ft(1) = ;
"陳陳陳珍陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳祖陳�"

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_C_M", "Tst_C_E", "Tst_C_I", ln, wd, ;
                "�ヤオ皚襯 ぅ��Ж硅", tt, ft

@ 1, 1 SAY "�"
@ 2, 1 SAY "�"
@ 3, 1 SAY "�"
@ WROWS()-1, 1 SAY "�"

@ 1, WCOLS()-2 SAY "�"
@ 2, WCOLS()-2 SAY "�"
@ 3, WCOLS()-2 SAY "�"
@ WROWS()-1, WCOLS()-2 SAY "�"

ACTIVATE WINDOW Tst_C_E
GO TOP

BROWSE FIELDS NUMB:H="",     ;
              CUS_NAME:H="", ;
              IS_DEF:H="", ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              WINDOW Tst_C_I IN WINDOW Tst_C_E COLOR (sch)

RELEASE WINDOW Tst_C_I
RELEASE WINDOW Tst_C_M
RELEASE WINDOW Tst_C_E
CLOSE DATABASES
RETURN
