*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� R_Index      ��о�｀砒┴ ��むォ ��瓱�           16.09.98 17:28:31 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    �エ�ぅ���罔� ������ ��щ ����諷.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE R_Index

SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT

PRIVATE f_list, i, err_lst, err_cod
DIMENSION f_list(4), err_lst(1)

f_list(1) = "STOCK"
f_list(2) = "LIST_DET"
f_list(3) = "ACCOUNT"
f_list(4) = "PERSONS"

err_cod = 0
err_lst(1) = 1705

FOR i = 1 TO ALEN(f_list)
  err_cod = 0
  USE (base_path+f_list(i)) EXCLUSIVE
  IF err_cod = 0
    REINDEX
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ���� "+f_list(i)+" 爛┃ぅ���罔� - OK!"
  ELSE
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ���� "+f_list(i)+" 爛┃ぅ���罔� - �� 磴���瘡 ���竍�碎 ぎ痰祚!"
  ENDIF
  USE
ENDFOR

SET PRINTER TO PRN:
SET DEVICE TO SCREEN

RETURN
