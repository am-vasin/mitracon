*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎ็จแโชจ TMP คจเฅชโฎเจจ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clr_Tmp

PRIVATE n, i, ad, err_sv, nm

m.n = ADIR(ad, m.tmpo_path+"*.*")
IF m.n = 0
  RETURN
ENDIF

m.err_sv = ON("ERROR")
ON ERROR =1
FOR m.i = 1 TO m.n
  m.nm = ad(m.i,1)
  IF UPPER(m.nm) == "PR_TMP.DBF" .OR. UPPER(m.nm) == "PR_TMP.CDX"
    LOOP
  ENDIF
  DELETE FILE ( m.tmpo_path+m.nm)
ENDFOR
ON ERROR &err_sv

RETURN
