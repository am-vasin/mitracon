*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Dir_List     ��о�｀砒┴ ��むォ ��瓱�           13.02.97 13:17:49 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ��爼�牀���┘ 甎�瓷� え爛�皰爬�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Dir_List
PARAMETER pth

PRIVATE a_w, n1, n2, i

n1 = ADIR(a_w, pth+"*", "D")
n2 = 0

FOR i = 1 TO n1
  IF "D" $ a_w(i,5)
    n2 = n2+1
    DIMENSION wk_list(n2)
    wk_list(n2) = PADR(a_w(i,1),8)+IIF(LEFT(a_w(i,1),1) = ".", "     ", " <DIR>")
  ENDIF
ENDFOR

IF n2 = 0
  DIMENSION wk_list(1)
  wk_list(1) = "              "
ENDIF

= ASORT(wk_list)

RETURN
