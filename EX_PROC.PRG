*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ - �÷�｀砒┴ か� �諷��� ├ 爛Θ��.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ex_Proc
PARAMETERS m_name, chr_code
PRIVATE c

IF TYPE("chr_code") = "N" .OR. TYPE("chr_code") = "C"
  c = chr_code
ELSE
  c = 27
ENDIF

KEYBOARD IIF(TYPE("c")="N", CHR(c), c)
IF .NOT. EMPTY(m_name)
  DEACTIVATE MENU (m_name)
ENDIF
RETURN

