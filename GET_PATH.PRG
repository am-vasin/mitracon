*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ���竍キ�� ぅ筮�皚��� �竄�.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_Path
PARAMETERS pth, k      && ��Б����ガ覃 �竄� � ��鈑 � ����� �竄ォ

PRIVATE sel_sav, ln

ln = LEN(pth)
IF .NOT. File_O(base_path+"def_path.dbf")
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
USE (base_path+"DEF_PATH")
LOCATE FOR UPPER(PADR(k, FSIZE("KEY"))) == UPPER(KEY)
IF FOUND()
  pth = ALLTRIM(DEF_PATH)
ENDIF
USE
SELE (sel_sav)

RETURN
