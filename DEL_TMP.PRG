*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ 磴��キ�� ∇ガキ�諷 ������.                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_Tmp
PARAMETERS pt         && �竄�

PRIVATE n, i, j, ad, nm, dl

n = ADIR(ad, pt+"*.*")
IF n = 0
  RETURN
ENDIF

FOR i = 1 TO n
  nm = ad(i,1)
  j = AT(".", nm)
  IF j # 0
    IF j < 5
      LOOP
    ENDIF
    nm = LEFT(nm, j-1)
  ENDIF
  FOR j = 1 TO LEN(nm)
    dl = .T.
    IF .NOT. ISDIGIT(SUBSTR(nm,j,1))
      dl = .F.
      EXIT
    ENDIF
  ENDFOR
  IF dl .AND. ad(i,3) < DATE()
    DELETE FILE (pt+ad(i,1))
  ENDIF
ENDFOR

RETURN
