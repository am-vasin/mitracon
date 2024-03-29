*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ 筮爼�牀����� 痰珮�矣琺 ��щ ����諷.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Bas_List
PARAMETERS pth      && 鉘� � ������ ��щ ����諷

PRIVATE _tags, _fields, fil_list, n, i, nm, p

IF FILE(pth+"bas_list.dbf") .OR. FILE(pth+"bas_list.fpt")
  DELETE FILE (pth+"bas_list.dbf")
  DELETE FILE (pth+"bas_list.fpt")
ENDIF

DO Set_Lwr
n = ADIR(fil_list,pth+"*.dbf")
IF n = 0
  RETURN
ENDIF

CREATE DBF (pth+"bas_list")  ;
    (  NAME      C(8),       ;
       TAGS      M,          ;
       FIELDS    M   )

FOR i = 1 TO n
  SELECT BAS_LIST
  nm = fil_list(i,1)
  p = AT(".",nm)
  nm = ALLTRIM(LEFT(nm,p-1))
  APPEND BLANK
  REPLACE NAME WITH nm
  SELECT 0
  USE (pth+nm) ALIAS WK
  DIMENSION _tags(1,3)
  IF P_Tags() # 0
    SELECT BAS_LIST
    SAVE TO MEMO TAGS ALL LIKE _tags
    SELECT WK
  ENDIF
  = AFIELDS(_fields)
  SELECT BAS_LIST
  SAVE TO MEMO FIELDS ALL LIKE _fields
  SELECT WK
  USE
ENDFOR
CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     �祗�罔� 筮爼�牀����� 痰珮�矣琺 CDX �����.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Tags
PRIVATE n, s
n = 0
DO WHILE .T.
  n = n+1
  s = TAG(n)
  IF EMPTY(s)
    EXIT
  ENDIF
  DIMENSION _tags(n,3)
  _tags(n,1) = s
  _tags(n,2) = SYS(14,n)
  _tags(n,3) = SYS(2021,n)
ENDDO
RETURN n-1
