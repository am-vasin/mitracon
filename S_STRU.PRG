*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� S_Stru       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             蹍������ 筮爼�牀����� 痰珮�矣琺 ��щ ����諷.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12/10/1998 様�
PROCEDURE S_Stru

PRIVATE pth1, pth2, arr, err

STORE SPACE(50) TO pth1, pth2
arr = 1
CLEAR
@ 10, 2 SAY "       鉘� � ��Д ����諷" GET pth1
@ 12, 2 SAY "�磴� �牀�メ皋碎 爛с�赳��" GET pth2
@ 14, 23 GET arr PICTURE "@*HT \! OK (Ctrl-W) ;\? �皖���碎瘴 (Esc) "
READ CYCLE
IF arr = 2
  RETURN
ENDIF
RELEASE arr
pth1 = ALLTRIM(pth1)
pth2 = ALLTRIM(pth2)

SET EXCLU OFF
CLEAR
DO Set_Lwr
n = ADIR(arr, pth1+"*.DBF")
err = 0
ON ERROR err = ERROR()
FOR i = 1 TO n
  WAIT WIND arr(i,1) NOWAIT
  err = 0
  USE (pth1+arr(i,1))
  IF err # 0
    WAIT WIND arr(i,1)+" �荐！� � "+ALLTIM(STR(err))
  ELSE
    COPY STRUCTURE TO (pth2+arr(i,1)) WITH CDX
  ENDIF
ENDFOR

ON ERROR
USE

RETURN

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
