*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ��≒�爿�� �ムイ�腮 ��甄イ�ィ� �砒モ�.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Send0_R

PRIVATE pthw, n, mss, wk_arr

pthw = tmpo_path+"transmit\"

n = ADIR(arr_w,pthw+"F_REPORT.A*")
IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�┴����� ��甄イ�ィ� �砒モ� �� 痺薀痰≪モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

FOR i = 1 TO n
  DO To_Disk WITH pthw+arr_w(i,1)
ENDFOR

RETURN
