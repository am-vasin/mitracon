*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� St_Send2     ��о�｀砒┴ ��むォ ��瓱�           27.06.97 10:02:27 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         �ムメ覊�� 痰����珥���� 甎�瓷� ������ �� �甅. �����お�.         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE St_Send2
PRIVATE mss, pthw

IF sys_char = "A"
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 �甅�↓�� �����お�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Prep_TD    && ��Г�ガ ┼� �腮��ガ 矗��Ж皚竡 え爛�皰爬�
pthw = tmpo_path+"transmit\"

DO Prep_St2 WITH wk_char, pthw

DO Arch WITH pthw, "ST_SEND2"

PRIVATE out_path
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "ST_SEND2.A")
    DELETE FILE (pthw+arr_w(i,1))
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "�磴� ��茱� ����襯?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"ST_SEND2", out_path+"ST_SEND2"  && ��縲��キ┘ �璢│�
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�  ��ぃ�皰→� 痰����珥���� 甎�瓷� ������ か� �ムイ�腮 �� �甅. �����お�.  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 27.06.97 10:52:22 夕
PROCEDURE Prep_St2
PARAMETERS w_ch, pth
PRIVATE arr_w, p, i

*
*     ��┘�硅
*
DO Wt_Mess WITH "��┘�硅"
USE (base_path+"ACCOUNT")
COPY TO (pth+"ACCOUNT")
DO Wt_Mess

CLOSE DATABASES

RETURN
