*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ 〓瘁����←キ�� ┐オ甌�.                                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Index_Ba
PARAMETERS pth, nam      && 鉘� � ������ ��щ ����諷

PRIVATE s, n, i, _tags, sw_tst, err_lst, pth_w

IF TYPE("pth") # "C"
  IF TYPE("base_path") # "C"
    pth_w = ""
  ELSE
    pth_w = base_path
  ENDIF
ELSE
  pth_w = pth
ENDIF

DIMENSION _tags(1), err_lst(1)
err_lst(1) = 1707
USE (pth_w+"bas_list.dbf")
sw_tst = TYPE("nam") = "N"   && ��Г�ガ, メ�� �モ cdx �����

SCAN
  IF TYPE("nam") = "C"
    IF .NOT. UPPER(ALLTRIM(nam)) == ALLTRIM(NAME)
      LOOP
    ENDIF
  ENDIF
  IF FILE(pth_w+ALLTRIM(NAME)+".cdx")
    IF sw_tst
      LOOP
    ENDIF
    DELETE FILE (pth_w+ALLTRIM(NAME)+".cdx")
  ENDIF
  IF EMPTY(TAGS)
    LOOP
  ENDIF
  RESTORE FROM MEMO TAGS ADDITIVE
  IF .NOT. WEXIST("Wt_Mess")
    DO Wt_Mess WITH "��瘁����←│�ガ ┃ぅ�瘠!"
  ENDIF
  n = ALEN(_tags,1)
  SELECT 0
  PRIVATE err_cod
  err_cod = 0
  DO WHILE .T.
    USE (pth_w+ALLTRIM(BAS_LIST.NAME)) ALIAS WK
    IF err_cod = 0
      EXIT
    ENDIF
    err_cod = 0
  ENDDO
  SELECT BAS_LIST
  FOR i = 1 TO n
    s = "INDEX ON "+_tags(i,2)+" TAG "+_tags(i,1)
    IF .NOT. EMPTY(_tags(i,3))
       s = s+" FOR "+_tags(i,3)
    ENDIF
    SELECT WK
    &s
    SELECT BAS_LIST
  ENDFOR
  SELECT WK
  USE
  SELECT BAS_LIST
ENDSCAN
USE
IF WEXIST("Wt_Mess")
  DO Wt_Mess
ENDIF
RETURN
