*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                ����� �ணࠬ�� ��ᬮ�� ������ �� ������.          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE New_V_N

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

IF .NOT. FILE_O(log_path+"N_T_N.DBF")   && ��� 䠩�� � १���⮬
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�஢�ન ������ �� �ந���������."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_N")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��宦����� �� ��������!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_N

PRIVATE ln, wd, tt, ft
DIMENSION tt(3), ft(1)
tt(1) = ;
"�������������������������������������������������������������������������"
tt(2) = ;
" N/��� �   ������                    ����.��������.����.�� ��.���.����.�   "
tt(3) = ;
"�������������������������������������������������������������������������"
ft(1) = ;
"�������������������������������������������������������������������������"

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_N_M", "Tst_N_E", "Tst_N_I", ln, wd, ;
                "����樨 � ����襭�� �����ᮬ �� ������", tt, ft

@ 1, 1 SAY "�"
@ 2, 1 SAY "�"
@ 3, 1 SAY "�"
@ WROWS()-1, 1 SAY "�"

@ 1, WCOLS()-2 SAY "�"
@ 2, WCOLS()-2 SAY "�"
@ 3, WCOLS()-2 SAY "�"
@ WROWS()-1, WCOLS()-2 SAY "�"

ACTIVATE WINDOW Tst_N_E
GO TOP
BROWSE FIELDS NUMB:H="",   ;
              f001 = PREFIX+" "+Nm_Part(NAME):30:H="",   ;
              NAL_S:H="",  ;
              NAL_R:H="",  ;
              MOV_F:H="",  ;
              NAL_F:H="",  ;
              NAL_B:H="",  ;
              NAL_B:H="",  ;
              NAL_B:H="",  ;
              NAL_SP:H="", ;
              MOV_SP:H="", ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              NOMENU WINDOW Tst_N_I IN WINDOW Tst_N_E COLOR (sch)
RELEASE WINDOW Tst_N_I
RELEASE WINDOW Tst_N_M
RELEASE WINDOW Tst_N_E
CLOSE DATABASES
RETURN