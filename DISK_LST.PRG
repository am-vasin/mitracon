*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ��爛ぅ�キ�� �ムョ�� ぎ痰祚�諷 え瓷��.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Disk_Lst

PRIVATE res, shab, i, n, err_sav, def_sav, err_sw, w

def_sav = SET("DEFAULT")+CURDIR()     && 皀�竕�� え爛�皰爬�
err_sav = ON("ERROR")                 && 皀�竕┤ �÷�｀砒┴ �荐｀�
shab    = "cdefghijklmnopqrstuvwxyz"  && ��゛�� - 甎�甌� 〓К. む��〓�
res     = ""                          && 爛с�赳�� - 甎�甌� ぎ痰. む��〓�
n       = LEN(shab)

err_sw   = .T.
ON ERROR err_sw = .F.

FOR i = 1 TO n
  w = SUBSTR(shab,i,1)
  err_sw = .T.
  SET DEFAULT TO (w+":\")
  IF err_sw
    res = res+w
  ENDIF
ENDFOR

ON ERROR &err_sav
SET DEFAULT TO (def_sav)
  
RETURN res
