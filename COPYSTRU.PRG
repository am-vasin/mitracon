*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� CopyStru     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ��爼�牀���┘ �竅硅� ��゛�� °щ ����諷.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/11/2000 様�
PROCEDURE CopyStru
PARAMETERS s_path, d_path

PRIVATE arr_list, n, i, err

SET EXCLU OFF
DO Set_Lwr

n = ADIR(arr_list, s_path+"*.DBF")
ON ERROR err = ERROR()
err = 0
FOR i = 1 TO n
  err = 0
  USE s_path+(arr_list(i,1))
  IF err = 0
    WAIT WINDOW arr_list(i,1) NOWAIT
    COPY STRUCTURE WITH CDX TO (d_path+arr_list(i,1))
  ELSE
    WAIT WINDOW arr_list(i,1)+" �荐！� "+ALLTRIM(STR(err))
  ENDIF
ENDFOR
ON ERROR
USE
RETURN
**********************************************************
PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32,"�、�ぅΗ┤������珀矣粤肄蓍裨跋鈿")
RETURN
