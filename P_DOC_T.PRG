*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� P_Doc_T      ��о�｀砒┴ ��むォ ��瓱�           03.02.97 23:11:25 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �ョ�碎 皀�痰� ぎ�祠キ��                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE P_Doc_T

PRIVATE txt, st, p_sw, i1, i2

DO Ini_Prn WITH ALLTRIM(PATTERN.DOC_PROMPT)+" "+ALLTRIM(STR(TXT_DOCS.DOC_NUM)),  ;
                TXT_DOCS.PAGE_LEN, 0, 1, TXT_DOCS.FONT, TXT_DOCS.F_FEED
txt = TXT_DOCS.D_TEXT

p_sw = .F.
DO WHILE LEN(txt) > 0

  i1 = AT(CHR(13)+CHR(10), txt)
  i2 = AT(CHR(12), txt)
  DO CASE
  CASE i1 = 0 .AND. i2 = 0
    st = txt
    txt = ""
    @ IIF(p_sw, 0, PROW()+1), 0 SAY st
  CASE i2 = 0 .OR. i1 < i2
    st = LEFT(txt, i1-1)
    txt = SUBSTR(txt, i1+2)
    @ IIF(p_sw, 0, PROW()+1), 0 SAY st
    p_sw = .F.
  CASE i1 = 0 .OR. i2 < i1
    st = LEFT(txt, i2-1)
    txt = SUBSTR(txt, i2+1)
    @ IIF(p_sw, 0, PROW()+1), 0 SAY st
    p_sw = .T.
  ENDCASE  

ENDDO

DO Term_Prn WITH ""

RETURN
