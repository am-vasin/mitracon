*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Pk_Base      ��о�｀砒┴ ��むォ ��瓱�           05.10.98 11:08:37 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      �����→� ������ ��щ ����諷.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Pk_Base
SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT

PRIVATE f_list, i, err_lst, err_cod
DIMENSION f_list(6), err_lst(1)

f_list(1) = "LIST_DET"
f_list(2) = "LIST_TIT"
f_list(3) = "RYN_L"
f_list(4) = "RYN_L_T"
f_list(5) = "RYN_1_L"
f_list(6) = "RYN_1_T"

err_cod = 0
err_lst(1) = 1705

FOR i = 1 TO ALEN(f_list)
  IF .NOT. File_O(base_path+f_list(i)+".DBF")
    LOOP
  ENDIF
  err_cod = 0
  USE (base_path+f_list(i)) EXCLUSIVE
  IF err_cod = 0
    PACK
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ���� "+f_list(i)+" 祚���→� - OK!"
  ELSE
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ���� "+f_list(i)+" 祚���→� - �� 磴���瘡 ���竍�碎 ぎ痰祚!"
  ENDIF
  USE
ENDFOR

SET PRINTER TO PRN:
SET DEVICE TO SCREEN

RETURN
